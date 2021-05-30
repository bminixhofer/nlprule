use serde::{Deserialize, Serialize};

use crate::types::*;
use thiserror::Error;

pub use suggest::Suggest;
pub use tokenize::Tokenize;
pub use transform::Transform;

pub mod suggest {
    use super::*;

    /// Correct a text by applying suggestions to it.
    /// In the case of multiple possible replacements, always chooses the first one.
    pub fn apply_suggestions(sentence: &Sentence, suggestions: &[Suggestion]) -> String {
        let mut offset: isize = -(sentence.span().char().start as isize);
        let mut chars: Vec<_> = sentence.text().chars().collect();

        for suggestion in suggestions {
            let replacement: Vec<_> = suggestion.replacements()[0].chars().collect();
            chars.splice(
                (suggestion.span().char().start as isize + offset) as usize
                    ..(suggestion.span().char().end as isize + offset) as usize,
                replacement.iter().cloned(),
            );
            offset = offset + replacement.len() as isize - suggestion.span().char().len() as isize;
        }

        chars.into_iter().collect()
    }

    pub trait Suggest {
        fn properties(&self) -> Properties {
            Properties::default()
        }

        fn property_guard(&self, sentence: &Sentence) -> Result<PropertyGuard, Error> {
            self.properties().build(sentence)
        }

        fn suggest(&self, sentence: &Sentence) -> Result<Vec<Suggestion>, Error>;

        fn correct(&self, sentence: &Sentence) -> Result<String, Error> {
            let suggestions = self.suggest(sentence)?;
            Ok(apply_suggestions(&sentence, &suggestions))
        }

        #[allow(unused_variables)]
        fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
            Ok(())
        }
    }

    impl<'a, T> Suggest for &'a T
    where
        T: Suggest,
    {
        fn properties(&self) -> Properties {
            (*self).properties()
        }

        fn property_guard(&self, sentence: &Sentence) -> Result<PropertyGuard, Error> {
            (*self).property_guard(sentence)
        }

        fn suggest(&self, sentence: &Sentence) -> Result<Vec<Suggestion>, Error> {
            (*self).suggest(sentence)
        }

        fn correct(&self, sentence: &Sentence) -> Result<String, Error> {
            (*self).correct(sentence)
        }

        fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
            (*self).test(tokenizer)
        }
    }
}

pub mod transform {
    use super::*;

    pub trait Transform {
        fn properties(&self) -> PropertiesMut {
            PropertiesMut::default()
        }

        fn property_guard(&self, sentence: &mut Sentence) -> Result<PropertyGuardMut, Error> {
            self.properties().build(sentence)
        }

        fn transform<'t>(&'t self, sentence: Sentence<'t>) -> Result<Sentence<'t>, Error>;

        #[allow(unused_variables)]
        fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
            Ok(())
        }
    }

    impl<'a, T> Transform for &'a T
    where
        T: Transform,
    {
        fn properties(&self) -> PropertiesMut {
            (*self).properties()
        }

        fn property_guard(&self, sentence: &mut Sentence) -> Result<PropertyGuardMut, Error> {
            (*self).property_guard(sentence)
        }

        fn transform<'t>(&'t self, sentence: Sentence<'t>) -> Result<Sentence<'t>, Error> {
            (*self).transform(sentence)
        }

        fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
            (*self).test(tokenizer)
        }
    }

    #[derive(Serialize, Deserialize)]
    pub struct Pipeline<T>(pub(super) T, pub(super) PropertiesMut);
}

pub mod tokenize {
    use super::*;

    pub trait Tokenize {
        fn properties(&self) -> PropertiesMut {
            PropertiesMut::default()
        }

        fn property_guard(&self, sentence: &mut Sentence) -> Result<PropertyGuardMut, Error> {
            self.properties().build(sentence)
        }

        fn tokenize<'t>(&'t self, text: &'t str) -> Box<dyn Iterator<Item = Sentence<'t>> + 't>;

        fn tokenize_sentence<'t>(&'t self, sentence: &'t str) -> Option<Sentence<'t>>;

        fn test(&self) -> Result<(), crate::Error> {
            Ok(())
        }
    }

    impl<'a, T> Tokenize for &'a T
    where
        T: Tokenize,
    {
        fn properties(&self) -> PropertiesMut {
            (*self).properties()
        }

        fn property_guard(&self, sentence: &mut Sentence) -> Result<PropertyGuardMut, Error> {
            (*self).property_guard(sentence)
        }

        fn tokenize<'t>(&'t self, text: &'t str) -> Box<dyn Iterator<Item = Sentence<'t>> + 't> {
            (*self).tokenize(text)
        }

        fn tokenize_sentence<'t>(&'t self, sentence: &'t str) -> Option<Sentence<'t>> {
            (*self).tokenize_sentence(sentence)
        }

        fn test(&self) -> Result<(), crate::Error> {
            (*self).test()
        }
    }

    #[derive(Serialize, Deserialize)]
    pub struct Pipeline<T>(pub(super) T, pub(super) PropertiesMut);
}

#[derive(Serialize, Deserialize)]
pub struct Pipeline<T>(T, PropertiesMut);

#[derive(Error, Debug)]
#[allow(missing_docs)]
pub enum Error {
    #[error("unset token property: {0:?}.")]
    Unset(Property),
    #[error("invalid pipeline: properties {0:?} are read without being written.")]
    InvalidPipeline(Vec<Property>),
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

    pub fn intersection(mut self, other: Bitset) -> Self {
        self.0 &= other.0;
        self
    }

    pub fn inverse(mut self) -> Self {
        self.0 = !self.0;
        self
    }

    pub fn into_iter<'a>(self) -> impl Iterator<Item = Property> + 'a {
        Property::properties().iter().filter_map(move |property| {
            if self.contains(property) {
                Some(*property)
            } else {
                None
            }
        })
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
    pub(crate) fn reads_without_write(&self) -> impl Iterator<Item = Property> {
        self.read_mask
            .intersection(self.write_mask.inverse())
            .into_iter()
    }

    pub fn union(mut self, properties: PropertiesMut) -> Self {
        self.read_mask = self.read_mask.union(properties.read_mask);
        self.write_mask = self.write_mask.union(properties.read_mask);

        self
    }

    pub fn chain(mut self, next: PropertiesMut) -> Self {
        let next_reads = next.read_mask.intersection(next.write_mask.inverse());
        let new_reads = next_reads.intersection(self.write_mask.inverse());

        self.read_mask = self.read_mask.union(new_reads);
        self.write_mask = self.write_mask.union(next.write_mask);
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

pub trait CreatePipe<T>: Sized {
    fn new(components: T) -> Result<Self, Error>;
}

macro_rules! make_subpipe {
    ($pipe:ty, $first:expr) => {
        Ok::<_, crate::Error>($first)
    };
    ($pipe:ty, $first:expr, $($name:expr),+) => {
        <$pipe>::new(($first, $($name,)+))
    }
}

macro_rules! impl_pipeline {
    ( $first:ident, $last:ident, $($name:ident),*) => {
        // Case 1: Tokenize -> Transform -> ... -> Transform
        impl<$first: Tokenize, $($name: Transform,)* $last: Transform> CreatePipe<($first, $($name,)* $last)> for tokenize::Pipeline<($first, $($name,)* $last)> {
            #[allow(non_snake_case, unused_mut)]
            fn new(components: ($first,  $($name,)* $last)) -> Result<Self, Error> {
                let (ref $first, $(ref $name,)* ref $last) = components;

                let mut properties = $first.properties();
                $(properties = properties.chain($name.properties());)*
                properties.chain($last.properties());

                if !properties.reads_without_write().next().is_none() {
                    return Err(Error::InvalidPipeline(properties.reads_without_write().collect()));
                }

                Ok(tokenize::Pipeline(components, properties))
            }
        }

        impl<$first: Tokenize, $($name: Transform,)* $last: Transform> Tokenize for tokenize::Pipeline<($first, $($name,)* $last)> {
            fn properties(&self) -> PropertiesMut {
                self.1
            }

            #[allow(non_snake_case)]
            fn tokenize<'t>(&'t self, text: &'t str) -> Box<dyn Iterator<Item = Sentence<'t>> + 't> {
                let (ref $first, $(ref $name,)* ref $last) = self.0;
                let sentences = $first.tokenize(text).map(move |mut sentence| {
                    $(sentence = $name.transform(sentence).unwrap();)*
                    sentence = $last.transform(sentence).unwrap();
                    sentence
                });

                Box::new(sentences)
            }

            #[allow(non_snake_case, unused_mut)]
            fn tokenize_sentence<'t>(&'t self, sentence: &'t str) -> Option<Sentence> {
                let (ref $first, $(ref $name,)* ref $last) = self.0;
                let mut sentence = $first.tokenize_sentence(sentence)?;
                $(sentence = $name.transform(sentence).unwrap();)*
                Some($last.transform(sentence).unwrap())
            }

            #[allow(non_snake_case)]
            fn test(&self) -> Result<(), crate::Error> {
                let (ref $first, $(ref $name,)* ref $last) = self.0;

                let subpipe = make_subpipe!(tokenize::Pipeline<_>, $first $(,$name)*)?;
                subpipe.test()?;

                $last.test(subpipe)?;

                Ok(())
            }
        }

        // Case 2: Transform -> ... -> Transform
        impl<$first: Transform, $($name: Transform,)* $last: Transform> CreatePipe<($first, $($name,)* $last)> for transform::Pipeline<($first, $($name,)* $last)> {
            #[allow(non_snake_case, unused_mut)]
            fn new(components: ($first,  $($name,)* $last)) -> Result<Self, Error> {
                let (ref $first, $(ref $name,)* ref $last) = components;

                let mut properties = $first.properties();
                $(properties = properties.chain($name.properties());)*
                properties.chain($last.properties());

                Ok(transform::Pipeline(components, properties))
            }
        }

        impl<$first: Transform, $($name: Transform,)* $last: Transform> Transform for transform::Pipeline<($first, $($name,)* $last)> {
            fn properties(&self) -> PropertiesMut {
                self.1
            }

            #[allow(non_snake_case)]
            fn transform<'t>(&'t self, mut sentence: Sentence<'t>) -> Result<Sentence<'t>, crate::properties::Error> {
                let (ref $first, $(ref $name,)* ref $last) = self.0;
                sentence = $first.transform(sentence)?;
                $(sentence = $name.transform(sentence)?;)*
                sentence = $last.transform(sentence)?;
                Ok(sentence)
            }

            #[allow(non_snake_case)]
            fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
                let (ref $first, $(ref $name,)* ref $last) = self.0;

                $first.test(&tokenizer)?;
                let tokenizer_pipe = tokenize::Pipeline::new((&tokenizer, $first))?;
                let subpipe = make_subpipe!(transform::Pipeline<_>, $($name,)* $last)?;

                subpipe.test(tokenizer_pipe)?;
                Ok(())
            }
        }

        // Case 3: Tokenize -> Transform -> ... -> Transform -> Suggest
        impl<$first: Tokenize, $($name: Transform,)* $last: Suggest> CreatePipe<($first, $($name,)* $last)> for Pipeline<($first, $($name,)* $last)> {
            #[allow(non_snake_case, unused_mut)]
            fn new(components: ($first,  $($name,)* $last)) -> Result<Self, Error> {
                let (ref $first, $(ref $name,)* ref $last) = components;

                let mut properties = $first.properties();
                $(properties = properties.chain($name.properties());)*
                properties.chain($last.properties().write(&[]));

                if !properties.reads_without_write().next().is_none() {
                    return Err(Error::InvalidPipeline(properties.reads_without_write().collect()));
                }

                Ok(Pipeline(components, properties))
            }
        }

        impl<$first: Tokenize, $($name: Transform,)* $last: Suggest> Pipeline<($first, $($name,)* $last)> {
            pub fn properties(&self) -> PropertiesMut {
                self.1
            }

            #[allow(non_snake_case, unused_mut)]
            pub fn suggest<'t>(&'t self, text: &'t str) -> impl Iterator<Item = Vec<Suggestion>> + 't {
                let (ref $first, $(ref $name,)* ref $last) = self.0;

                let sentences = $first.tokenize(text).map(move |mut sentence| {
                    $(sentence = $name.transform(sentence).unwrap();)*
                    $last.suggest(&sentence).unwrap()
                });

                sentences
            }

            #[allow(non_snake_case, unused_mut)]
            pub fn correct<'t>(&'t self, text: &'t str) -> impl Iterator<Item = String> + 't {
                let (ref $first, $(ref $name,)* ref $last) = self.0;

                let sentences = $first.tokenize(text).map(move |mut sentence| {
                    $(sentence = $name.transform(sentence).unwrap();)*
                    $last.correct(&sentence).unwrap()
                });

                sentences
            }

            #[allow(non_snake_case)]
            pub fn test(&self) -> Result<(), crate::Error> {
                let (ref $first, $(ref $name,)* ref $last) = self.0;

                let subpipe = make_subpipe!(tokenize::Pipeline<_>, $first $(,$name)*)?;
                subpipe.test()?;

                $last.test(subpipe)?;

                Ok(())
            }
        }
    };
}

impl_pipeline! { A, B, }
impl_pipeline! { A, C, B  }
impl_pipeline! { A, D, B, C }
impl_pipeline! { A, E, B, C, D }
