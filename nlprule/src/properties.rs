use serde::{Deserialize, Serialize};

use crate::types::*;
use thiserror::Error;

pub trait ReadProperties {
    fn properties(&self) -> Properties {
        Properties::default()
    }

    fn property_guard(&self, sentence: &Sentence) -> Result<PropertyGuard, Error> {
        self.properties().build(sentence)
    }
}

pub trait WriteProperties {
    fn properties(&self) -> PropertiesMut {
        PropertiesMut::default()
    }

    fn property_guard(&self, sentence: &mut Sentence) -> Result<PropertyGuardMut, Error> {
        self.properties().build(sentence)
    }
}

#[derive(Error, Debug)]
#[allow(missing_docs)]
pub enum Error {
    #[error("unset token property: {0:?}")]
    Unset(Property),
}

#[derive(Debug, Clone, Copy)]
pub enum Property {
    Tags = 0,
    Chunks = 1,
}

impl Property {
    pub fn properties() -> &'static [Property] {
        &[Property::Tags, Property::Chunks]
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, Default)]
struct Bitset(u16);

impl Bitset {
    pub fn insert(&mut self, value: Property) {
        self.0 |= 1 << (value as u16);
    }

    pub fn contains(&self, value: &Property) -> bool {
        self.0 & (1 << (*value as u16)) != 0
    }

    pub fn union(mut self, other: Bitset) -> Self {
        self.0 |= other.0;
        self
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default)]
pub struct Properties {
    read_mask: Bitset,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default)]
pub struct PropertiesMut {
    read_mask: Bitset,
    write_mask: Bitset,
}

impl std::iter::FromIterator<Properties> for Properties {
    fn from_iter<T: IntoIterator<Item = Properties>>(iter: T) -> Self {
        let mut out = Properties::default();

        for properties in iter {
            out = out.union(properties)
        }

        out
    }
}

impl std::iter::FromIterator<PropertiesMut> for PropertiesMut {
    fn from_iter<T: IntoIterator<Item = PropertiesMut>>(iter: T) -> Self {
        let mut out = PropertiesMut::default();

        for properties in iter {
            out = out.union(properties)
        }

        out
    }
}

impl Properties {
    pub fn read(mut self, properties: &[Property]) -> Self {
        for property in properties {
            self.read_mask.insert(*property);
        }

        self
    }

    pub fn write(self, properties: &[Property]) -> PropertiesMut {
        let mut write_mask = Bitset::default();
        let mut read_mask = self.read_mask;

        for property in properties {
            // write implies read
            read_mask.insert(*property);
            write_mask.insert(*property);
        }

        PropertiesMut {
            read_mask,
            write_mask,
        }
    }

    pub fn union(mut self, properties: Properties) -> Self {
        self.read_mask = self.read_mask.union(properties.read_mask);

        self
    }

    pub fn build(&self, sentence: &Sentence) -> Result<PropertyGuard, Error> {
        for property in Property::properties() {
            if self.read_mask.contains(property) {
                match *property {
                    Property::Tags => {
                        if sentence.first().tags.is_none() {
                            return Err(Error::Unset(Property::Tags));
                        }
                    }
                    Property::Chunks => {
                        if sentence.first().chunks.is_none() {
                            return Err(Error::Unset(Property::Chunks));
                        }
                    }
                }
            }
        }

        Ok(PropertyGuard {
            read_mask: self.read_mask,
        })
    }
}

impl PropertiesMut {
    pub fn union(mut self, properties: PropertiesMut) -> Self {
        self.read_mask = self.read_mask.union(properties.read_mask);
        self.write_mask = self.write_mask.union(properties.read_mask);

        self
    }

    pub fn build(&self, sentence: &mut Sentence) -> Result<PropertyGuardMut, Error> {
        for property in Property::properties() {
            if self.write_mask.contains(property) {
                match property {
                    Property::Tags => {
                        if sentence.first().tags.is_none() {
                            sentence
                                .iter_mut()
                                .for_each(|token| token.tags = Some(Tags::default()));
                        }
                    }
                    Property::Chunks => {
                        if sentence.first().chunks.is_none() {
                            sentence
                                .iter_mut()
                                .for_each(|token| token.chunks = Some(Vec::default()));
                        }
                    }
                }
            }
        }

        for property in Property::properties() {
            if self.read_mask.contains(property) {
                match *property {
                    Property::Tags => {
                        if sentence.first().tags.is_none() {
                            return Err(Error::Unset(Property::Tags));
                        }
                    }
                    Property::Chunks => {
                        if sentence.first().chunks.is_none() {
                            return Err(Error::Unset(Property::Chunks));
                        }
                    }
                }
            }
        }

        Ok(PropertyGuardMut {
            read_mask: self.read_mask,
            write_mask: self.write_mask,
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PropertyGuard {
    read_mask: Bitset,
}

#[derive(Debug, Copy, Clone)]
pub struct PropertyGuardMut {
    read_mask: Bitset,
    write_mask: Bitset,
}

impl PropertyGuard {
    pub fn chunks<'a>(&self, token: &'a Token) -> Result<&'a [String], Error> {
        match (
            token.chunks.as_deref(),
            self.read_mask.contains(&Property::Chunks),
        ) {
            (Some(chunks), true) => Ok(chunks),
            _ => Err(Error::Unset(Property::Chunks)),
        }
    }

    pub fn tags<'a, 't>(&self, token: &'a Token<'t>) -> Result<&'a Tags<'t>, Error> {
        match (
            token.tags.as_ref(),
            self.read_mask.contains(&Property::Tags),
        ) {
            (Some(tags), true) => Ok(tags),
            _ => Err(Error::Unset(Property::Tags)),
        }
    }
}

impl PropertyGuardMut {
    pub fn chunks<'a>(&self, token: &'a Token) -> Result<&'a [String], Error> {
        match (
            token.chunks.as_deref(),
            self.read_mask.contains(&Property::Chunks),
        ) {
            (Some(chunks), true) => Ok(chunks),
            _ => Err(Error::Unset(Property::Chunks)),
        }
    }

    pub fn tags<'a, 't>(&self, token: &'a Token<'t>) -> Result<&'a Tags<'t>, Error> {
        match (
            token.tags.as_ref(),
            self.read_mask.contains(&Property::Tags),
        ) {
            (Some(tags), true) => Ok(tags),
            _ => Err(Error::Unset(Property::Tags)),
        }
    }

    pub fn chunks_mut<'a, 't>(
        &self,
        token: &'a mut Token<'t>,
    ) -> Result<&'a mut Vec<String>, Error> {
        match (
            token.chunks.as_mut(),
            self.write_mask.contains(&Property::Chunks),
        ) {
            (Some(chunks), true) => Ok(chunks),
            _ => Err(Error::Unset(Property::Chunks)),
        }
    }

    pub fn tags_mut<'a, 't>(&self, token: &'a mut Token<'t>) -> Result<&'a mut Tags<'t>, Error> {
        match (
            token.tags.as_mut(),
            self.write_mask.contains(&Property::Tags),
        ) {
            (Some(tags), true) => Ok(tags),
            _ => Err(Error::Unset(Property::Tags)),
        }
    }

    pub fn downgrade(self) -> PropertyGuard {
        PropertyGuard {
            read_mask: self.read_mask,
        }
    }
}

// pub trait Transform {
//     fn transform<'t>(&'t self, sentences: SentenceIter<'t>) -> SentenceIter<'t>;

//     fn in_properties(&self) ->
// }

// pub struct Pipeline<T, P>(T, P);

// type SentenceIter<'t> = Box<dyn Iterator<Item = Sentence<'t>>>;

// macro_rules! impl_pipeline {
//     ( $first:ident, $($name:ident),+) => {
//         impl<$first: Tokenize, $($name: Transform,)+> Tokenize for Pipeline<($first, $($name,)+)> {
//             #[allow(non_snake_case)]
//             fn tokenize<'t>(&'t self, text: &'t str) -> SentenceIter<'t> {
//                 let (ref $first, $(ref $name),+) = self.0;
//                 let sentences = $first.tokenize(text);
//                 $(let sentences = $name.transform(sentences);)+
//                 sentences
//             }
//         }

//         impl<$first: Transform, $($name: Transform,)+> Transform for Pipeline<($first, $($name,)+)> {
//             #[allow(non_snake_case)]
//             fn transform<'t>(&'t self, sentences: SentenceIter<'t>) -> SentenceIter<'t> {
//                 let (ref $first, $(ref $name),+) = self.0;
//                 let sentences = $first.transform(sentences);
//                 $(let sentences = $name.transform(sentences);)+
//                 sentences
//             }
//         }
//     };
// }

// impl_pipeline! { A, B }
// impl_pipeline! { A, B, C }
// impl_pipeline! { A, B, C, D }
// impl_pipeline! { A, B, C, D, E }
