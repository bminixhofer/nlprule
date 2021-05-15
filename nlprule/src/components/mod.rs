use std::{
    io::{BufReader, Read, Write},
    path::Path,
};

use fs_err::File;
use serde::{de::DeserializeOwned, Serialize};

pub mod chunker;
pub mod multiword_tagger;
pub mod rules;
pub mod tagger;
pub mod tokenizer;

pub trait Component: Serialize + DeserializeOwned {
    fn name() -> &'static str;

    fn new<P: AsRef<Path>>(p: P) -> Result<Self, crate::Error> {
        let reader = BufReader::new(File::open(p.as_ref())?);
        Ok(Self::from_reader(reader)?)
    }

    fn from_reader<R: Read>(reader: R) -> Result<Self, crate::Error> {
        Ok(bincode::deserialize_from(reader)?)
    }

    fn to_writer<W: Write>(&self, writer: W) -> Result<(), crate::Error> {
        Ok(bincode::serialize_into(writer, self)?)
    }
}
