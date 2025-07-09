use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct CompactSize {
    pub value: u64,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BitcoinError {
    InsufficientBytes,
    InvalidFormat,
}

impl CompactSize {
    pub fn new(value: u64) -> Self {
        return CompactSize{value};
        // TODO: Construct a CompactSize from a u64 value
    }
    

    pub fn to_bytes(&self) -> Vec<u8> {
        match self.value {
            0x00..=0xFC => vec![self.value as u8],

            0xFD..=0xFFFF => {
                let mut bytes = vec![0xFD];
                bytes.extend_from_slice(&(self.value as u16).to_le_bytes());
                bytes
            }
            
            0x10000..=0xFFFFFFFF => {
                let mut bytes = vec![0xFE];
                bytes.extend_from_slice(&(self.value as u32).to_le_bytes());
                bytes
            }

            _ => {
                let mut bytes = vec![0xFE];
                bytes.extend_from_slice(&(self.value as u32).to_le_bytes());
                bytes
            }
        }
        // TODO: Encode according to Bitcoin's CompactSize format:
        // [0x00–0xFC] => 1 byte
        // [0xFDxxxx] => 0xFD + u16 (2 bytes)
        // [0xFExxxxxxxx] => 0xFE + u32 (4 bytes)
        // [0xFFxxxxxxxxxxxxxxxx] => 0xFF + u64 (8 bytes)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        if bytes.is_empty() {
            return Err(BitcoinError::InsufficientBytes);
        }

        match bytes[0] {
            0x00..=0xFC => {
                Ok((CompactSize::new(bytes[0] as u64), 1))
            }

            0xFD => {
                if bytes.len() < 3 {
                    return Err(BitcoinError::InsufficientBytes);
                }
                let value = u16::from_le_bytes([bytes[1], bytes[2]]) as u64;
                if value < 0xFD {
                    return Err(BitcoinError::InvalidFormat);
                }
                
                Ok((CompactSize::new(value), 3))
            }

            0xFE => {
                if bytes.len() < 5 {
                    return Err(BitcoinError:: InsufficientBytes);
                }
                let value = u32::from_le_bytes([bytes[1], bytes[2], bytes[3], bytes[4]]) as u64;
                if value < 65536 {
                    return Err(BitcoinError::InvalidFormat);
                }
                Ok((CompactSize::new(value), 5))
            }

            0xFF => {
                if bytes.len() < 9 {
                    return Err(BitcoinError::InsufficientBytes);
                }
                let value = u64::from_le_bytes([
                    bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7], bytes[8]
                ]);
                if value < 4294967296 {
                    return Err(BitcoinError::InvalidFormat);
                }
                Ok((CompactSize::new(value), 9))
            }
        }
        // TODO: Decode CompactSize, returning value and number of bytes consumed.
        // First check if bytes is empty.
        // Check that enough bytes are available based on prefix.
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Txid(pub [u8; 32]);

impl Serialize for Txid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let hex_string = hex::encode(&self.0);
        serializer.serialize_str(&hex_string)
        // TODO: Serialize as a hex-encoded string (32 bytes => 64 hex characters)
    }
}

impl<'de> Deserialize<'de> for Txid {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let hex_string = String::deserialize(deserializer)?;
        let bytes = hex::decode(&hex_string)
            .map_err(|e| serde::de::Error::custom(format!("Invalid hex string: {}", e)))?;

        if bytes.len() != 32 {
            return Err(serde::de::Error::custom(format!(
                "Expected 32 bytes, got {}",
                bytes.len()
            )));
        }

        let mut array = [0u8; 32];
        array.copy_from_slice(&bytes);

        Ok(Txid(array))
        // TODO: Parse hex string into 32-byte array
        // Use `hex::decode`, validate length = 32
        
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct OutPoint {
    pub txid: Txid,
    pub vout: u32,
}

impl OutPoint {
    pub fn new(txid: [u8; 32], vout: u32) -> Self {
        Self {
            txid: Txid(txid),
            vout,
        }
       
        // TODO: Create an OutPoint from raw txid bytes and output index
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(36);   
    
        bytes.extend_from_slice(&self.txid.0);   
  
        bytes.extend_from_slice(&self.vout.to_le_bytes());
    
        bytes
        // TODO: Serialize as: txid (32 bytes) + vout (4 bytes, little-endian)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        if bytes.len() < 36 {
        return Err(BitcoinError::InsufficientBytes)
          
    }
    let mut txid = [0u8; 32];
    txid.copy_from_slice(&bytes[0..32]);

    let vout_bytes = &bytes[32..36];
    let vout = u32::from_le_bytes([
        vout_bytes[0],
        vout_bytes[1], 
        vout_bytes[2],
        vout_bytes[3]
    ]);
    
    let outpoint = Self { txid:Txid(txid), vout };
    Ok((outpoint, 36))
    
        // TODO: Deserialize 36 bytes: txid[0..32], vout[32..36]
        // Return error if insufficient bytes
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Script {
    pub bytes: Vec<u8>,
}

impl Script {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self{
            bytes
        }
        // TODO: Simple constructor
    }

    pub fn to_bytes(&self) -> Vec<u8> {
         let mut result = Vec::new();
         let length = CompactSize::new(self.bytes.len() as u64);
         result.extend_from_slice(&length.to_bytes());
         result.extend_from_slice(&self.bytes);
         result
        // TODO: Prefix with CompactSize (length), then raw bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        let (length, bytes_consumed) = CompactSize::from_bytes(bytes)?;
        let script_length = length.value as usize;

        if bytes.len() < bytes_consumed + script_length {
            return Err(BitcoinError::InsufficientBytes);
        }

        let script_bytes = bytes[bytes_consumed..bytes_consumed + script_length].to_vec();
        let script = Script::new(script_bytes);

        Ok((script, bytes_consumed + script_length))
        // TODO: Parse CompactSize prefix, then read that many bytes
        // Return error if not enough bytes
    }
}

impl Deref for Script {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.bytes
        // TODO: Allow &Script to be used as &[u8]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct TransactionInput {
    pub previous_output: OutPoint,
    pub script_sig: Script,
    pub sequence: u32,
}

impl TransactionInput {
    pub fn new(previous_output: OutPoint, script_sig: Script, sequence: u32) -> Self {
        Self {
            previous_output,
            script_sig,
            sequence
        }
        // TODO: Basic constructor
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.previous_output.to_bytes());
        bytes.extend_from_slice(&self.script_sig.to_bytes());
        bytes.extend_from_slice(&self.sequence.to_le_bytes());
        bytes
        // TODO: Serialize: OutPoint + Script (with CompactSize) + sequence (4 bytes LE)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        let mut offset = 0;

        let (previous_output, outpoint_size) = OutPoint::from_bytes(&bytes[offset..])?;
        offset += outpoint_size;

        let (script_sig, script_size) = Script::from_bytes(&bytes[offset..])?;
        offset += script_size;

        if bytes.len() < offset + 4 {
            return Err(BitcoinError::InsufficientBytes);
        }

        let sequence = u32::from_le_bytes([
            bytes[offset],
            bytes[offset + 1],
            bytes[offset + 2],
            bytes[offset + 3],
        ]);
        offset += 4;

        let input = TransactionInput::new(previous_output, script_sig, sequence);
        Ok((input, offset))
     
        // TODO: Deserialize in order:
        // - OutPoint (36 bytes)
        // - Script (with CompactSize)
        // - Sequence (4 bytes)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct BitcoinTransaction {
    pub version: u32,
    pub inputs: Vec<TransactionInput>,
    pub lock_time: u32,
}

impl BitcoinTransaction {
    pub fn new(version: u32, inputs: Vec<TransactionInput>, lock_time: u32) -> Self {
        Self {
            version,
            inputs,
            lock_time
        }
        // TODO: Construct a transaction from parts
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        // TODO: Format:
        // - version (4 bytes LE)
        // - CompactSize (number of inputs)
        // - each input serialized
        // - lock_time (4 bytes LE)
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), BitcoinError> {
        // TODO: Read version, CompactSize for input count
        // Parse inputs one by one
        // Read final 4 bytes for lock_time
    }
}

impl fmt::Display for BitcoinTransaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Format a user-friendly string showing version, inputs, lock_time
        // Display scriptSig length and bytes, and previous output info
    }
}
