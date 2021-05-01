use std::iter;

use crate::{properties::*, tokenizer::tag::Tagger, types::*, utils::regex::Regex};
use enum_dispatch::enum_dispatch;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use unicase::UniCase;

type Context<'a, 't> = (&'a MatchSentence<'t>, &'a MatchGraph<'t>);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Matcher {
    pub matcher: either::Either<either::Either<String, GraphId>, Regex>,
    pub negate: bool,
    pub case_sensitive: bool,
    pub empty_always_false: bool,
}

impl Matcher {
    pub fn is_slice_match<S: AsRef<str>>(
        &self,
        input: &[S],
        context: Option<Context>,
        case_sensitive: Option<bool>,
    ) -> bool {
        input
            .iter()
            .any(|x| self.is_match(x.as_ref(), context, case_sensitive))
    }

    pub fn is_match(
        &self,
        input: &str,
        context: Option<Context>,
        case_sensitive: Option<bool>,
    ) -> bool {
        if input.is_empty() {
            return if self.empty_always_false {
                false
            } else {
                self.negate
            };
        }
        let case_sensitive = case_sensitive.unwrap_or(self.case_sensitive);

        let matches = match &self.matcher {
            either::Left(string_or_idx) => match string_or_idx {
                either::Left(string) => {
                    if case_sensitive {
                        string.as_str() == input
                    } else {
                        UniCase::new(string) == UniCase::new(input)
                    }
                }
                either::Right(id) => {
                    let (sentence, graph) =
                        context.expect("context must be set for context-dependent matcher");

                    graph
                        .by_id(*id)
                        .tokens(sentence)
                        .next()
                        .map_or(false, |token| {
                            if case_sensitive {
                                token.as_str() == input
                            } else {
                                UniCase::new(token.as_str()) == UniCase::new(input)
                            }
                        })
                }
            },
            either::Right(regex) => regex.is_match(input),
        };

        if self.negate {
            !matches
        } else {
            matches
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub(crate) struct TextMatcher {
    pub(crate) matcher: Matcher,
    pub(crate) set: Option<DefaultHashSet<WordIdInt>>,
}

impl TextMatcher {
    pub fn is_match(
        &self,
        word_id: &WordId,
        context: Option<Context>,
        case_sensitive: Option<bool>,
    ) -> bool {
        if self.set.is_none() {
            return self
                .matcher
                .is_match(word_id.as_str(), context, case_sensitive);
        }

        if let Some(id) = word_id.id() {
            self.set.as_ref().unwrap().contains(id)
        } else {
            self.matcher
                .is_match(word_id.as_str(), context, case_sensitive)
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PosMatcher {
    pub mask: Vec<bool>,
}

impl PosMatcher {
    pub fn is_match(&self, pos: &PosId) -> bool {
        self.mask[pos.id().value() as usize]
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct WordDataMatcher {
    pub(crate) pos_matcher: Option<PosMatcher>,
    pub(crate) inflect_matcher: Option<TextMatcher>,
}

impl WordDataMatcher {
    pub fn is_match<'t, I: Iterator<Item = &'t WordData<'t>>>(
        &self,
        mut input: I,
        context: Option<Context>,
        case_sensitive: Option<bool>,
    ) -> bool {
        input.any(|x| {
            let pos_matches = self
                .pos_matcher
                .as_ref()
                .map_or(true, |m| m.is_match(x.pos()));

            // matching part-of-speech tag is faster than inflection, so check POS first and early exit if it doesn't match
            if !pos_matches {
                return false;
            }

            let inflect_matches = self
                .inflect_matcher
                .as_ref()
                .map_or(true, |m| m.is_match(x.lemma(), context, case_sensitive));

            inflect_matches
        })
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Quantifier {
    pub min: usize,
    pub max: usize,
}

#[enum_dispatch]
pub trait Atomable: Send + Sync {
    fn is_match(&self, context: Context, position: usize)
        -> Result<bool, crate::properties::Error>;

    fn properties(&self) -> Properties {
        Properties::default()
    }
}

#[enum_dispatch(Atomable, ReadProperties)]
#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Atom {
    ChunkAtom(concrete::ChunkAtom),
    SpaceBeforeAtom(concrete::SpaceBeforeAtom),
    TextAtom(concrete::TextAtom),
    WordDataAtom(concrete::WordDataAtom),
    TrueAtom,
    FalseAtom,
    AndAtom,
    OrAtom,
    NotAtom,
    OffsetAtom,
}

pub mod concrete {
    use super::{Atomable, Context, Matcher, Properties, Property, TextMatcher, WordDataMatcher};
    use lazy_static::lazy_static;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct TextAtom {
        pub(crate) matcher: TextMatcher,
    }

    impl Atomable for TextAtom {
        fn is_match(
            &self,
            context: Context,
            position: usize,
        ) -> Result<bool, crate::properties::Error> {
            let (sentence, _) = context;

            Ok(self
                .matcher
                .is_match(&sentence.index(position).text(), Some(context), None))
        }
    }

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct ChunkAtom {
        pub(crate) matcher: Matcher,
    }

    impl Atomable for ChunkAtom {
        fn is_match(
            &self,
            context: Context,
            position: usize,
        ) -> Result<bool, crate::properties::Error> {
            let (sentence, _) = context;

            Ok(self.matcher.is_slice_match(
                sentence.guard().chunks(sentence.index(position))?,
                Some(context),
                None,
            ))
        }

        fn properties(&self) -> Properties {
            lazy_static! {
                static ref PROPERTIES: Properties = Properties::default().read(&[Property::Chunks]);
            }
            *PROPERTIES
        }
    }

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct SpaceBeforeAtom {
        pub(crate) value: bool,
    }

    impl Atomable for SpaceBeforeAtom {
        fn is_match(
            &self,
            context: Context,
            position: usize,
        ) -> Result<bool, crate::properties::Error> {
            let (sentence, _) = context;

            Ok(sentence.index(position).has_space_before() == self.value)
        }
    }

    #[derive(Debug, Serialize, Deserialize, Clone)]
    pub struct WordDataAtom {
        pub(crate) matcher: WordDataMatcher,
        pub(crate) case_sensitive: bool,
    }

    impl Atomable for WordDataAtom {
        fn is_match(
            &self,
            context: Context,
            position: usize,
        ) -> Result<bool, crate::properties::Error> {
            let (sentence, _) = context;
            let tags = sentence.guard().tags(sentence.index(position))?.iter();

            Ok(self
                .matcher
                .is_match(tags, Some(context), Some(self.case_sensitive)))
        }

        fn properties(&self) -> Properties {
            lazy_static! {
                static ref PROPERTIES: Properties = Properties::default().read(&[Property::Tags]);
            }
            *PROPERTIES
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct TrueAtom {}

impl Atomable for TrueAtom {
    fn is_match(
        &self,
        _context: Context,
        _position: usize,
    ) -> Result<bool, crate::properties::Error> {
        Ok(true)
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct FalseAtom {}

impl Atomable for FalseAtom {
    fn is_match(
        &self,
        _context: Context,
        _position: usize,
    ) -> Result<bool, crate::properties::Error> {
        Ok(false)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct AndAtom {
    pub(crate) atoms: Vec<Atom>,
}

impl Atomable for AndAtom {
    fn is_match(
        &self,
        context: Context,
        position: usize,
    ) -> Result<bool, crate::properties::Error> {
        for atom in &self.atoms {
            if !atom.is_match(context, position)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    fn properties(&self) -> Properties {
        self.atoms.iter().map(|x| x.properties()).collect()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct OrAtom {
    pub(crate) atoms: Vec<Atom>,
}

impl Atomable for OrAtom {
    fn is_match(
        &self,
        context: Context,
        position: usize,
    ) -> Result<bool, crate::properties::Error> {
        for atom in &self.atoms {
            if atom.is_match(context, position)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn properties(&self) -> Properties {
        self.atoms.iter().map(|x| x.properties()).collect()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NotAtom {
    pub(crate) atom: Box<Atom>,
}

impl Atomable for NotAtom {
    fn is_match(
        &self,
        context: Context,
        position: usize,
    ) -> Result<bool, crate::properties::Error> {
        Ok(!self.atom.is_match(context, position)?)
    }

    fn properties(&self) -> Properties {
        self.atom.properties()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct OffsetAtom {
    pub(crate) atom: Box<Atom>,
    pub(crate) offset: isize,
}

impl Atomable for OffsetAtom {
    fn is_match(
        &self,
        context: Context,
        position: usize,
    ) -> Result<bool, crate::properties::Error> {
        let (sentence, _) = context;
        let new_position = position as isize + self.offset;

        Ok(
            if new_position < 0 || (new_position as usize) >= sentence.len() {
                false
            } else {
                self.atom.is_match(context, new_position as usize)?
            },
        )
    }

    fn properties(&self) -> Properties {
        self.atom.properties()
    }
}

#[derive(Debug, Default, Clone)]
pub struct Group {
    pub span: Span,
}

impl Group {
    pub fn new(span: Span) -> Self {
        Group { span }
    }

    pub fn tokens<'a, 't>(
        &'a self,
        sentence: &'t MatchSentence,
    ) -> impl DoubleEndedIterator<Item = &'t Token<'t>> {
        let start = self.span.char().start;
        let end = self.span.char().end;

        sentence.iter().filter(move |x| {
            x.span().char().end > x.span().char().start // special tokens with zero range (e. g. SENT_START) can not be part of groups
                && x.span().char().start >= start
                && x.span().char().end <= end
        })
    }

    pub fn text<'a>(&self, sentence: &'a MatchSentence<'a>) -> &'a str {
        if self.span.char().start >= self.span.char().end {
            return "";
        }

        sentence.slice(self.span.clone())
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(transparent)]
pub struct GraphId(pub usize);

impl GraphId {
    /// Returns an iterator from the lower bound (inclusive) to the upper bound (inclusive).
    /// Important: this assumes both ids index into the same graph, otherwise the generated ids
    /// might not be valid!
    pub fn range(lower: &GraphId, upper: &GraphId) -> impl DoubleEndedIterator<Item = GraphId> {
        (lower.0..upper.0 + 1).map(GraphId)
    }
}

#[derive(Debug, Clone)]
pub struct MatchSentence<'t> {
    sentence: &'t Sentence<'t>,
    guard: PropertyGuard,
}

impl<'t> MatchSentence<'t> {
    pub fn new(sentence: &'t Sentence<'t>, guard: PropertyGuard) -> Self {
        MatchSentence { sentence, guard }
    }

    pub fn index(&self, index: usize) -> &Token {
        match index {
            0 => &*crate::types::SENT_START,
            i => &self.sentence.tokens()[i - 1],
        }
    }

    pub fn iter(&'t self) -> impl DoubleEndedIterator<Item = &'t Token> {
        iter::once(self.index(0)).chain(self.sentence.iter())
    }

    pub fn len(&self) -> usize {
        self.sentence.len() + 1
    }

    pub fn text(&self) -> &str {
        self.sentence.text()
    }

    pub fn slice(&self, span: Span) -> &str {
        let span = span.lshift(self.span().start());
        &self.text()[span.byte().clone()]
    }

    pub fn tagger(&self) -> &'t Tagger {
        self.sentence.tagger()
    }

    pub fn guard(&self) -> &PropertyGuard {
        &self.guard
    }

    pub fn span(&self) -> &Span {
        self.sentence.span()
    }
}

#[derive(Debug, Clone)]
pub struct MatchGraph<'t> {
    groups: Vec<Group>,
    id_to_idx: &'t DefaultHashMap<GraphId, usize>,
}

lazy_static! {
    static ref EMPTY_MAP: DefaultHashMap<GraphId, usize> = DefaultHashMap::default();
}

impl<'t> Default for MatchGraph<'t> {
    fn default() -> Self {
        MatchGraph {
            groups: Vec::new(),
            id_to_idx: &(*EMPTY_MAP),
        }
    }
}

impl<'t> MatchGraph<'t> {
    pub fn new(groups: Vec<Group>, id_to_idx: &'t DefaultHashMap<GraphId, usize>) -> Self {
        MatchGraph { groups, id_to_idx }
    }

    pub fn by_index(&self, index: usize) -> &Group {
        &self.groups[index]
    }

    pub fn by_id(&self, id: GraphId) -> &Group {
        &self.groups[self.get_index(id)]
    }

    pub fn get_index(&self, id: GraphId) -> usize {
        *self
            .id_to_idx
            .get(&id)
            .expect("only valid graph indices exist")
    }

    pub fn groups(&self) -> &[Group] {
        &self.groups[..]
    }

    pub fn fill_empty(&mut self, sentence: &MatchSentence) {
        let mut start = self
            .groups
            .iter()
            .find_map(|x| x.tokens(&sentence).next().map(|token| token.span().start()))
            .expect("graph must contain at least one token");

        let mut end = self
            .groups
            .iter()
            .rev()
            .find_map(|x| {
                x.tokens(&sentence)
                    .next_back()
                    .map(|token| token.span().end())
            })
            .expect("graph must contain at least one token");

        let group_tokens: Vec<_> = self
            .groups
            .iter()
            .map(|x| x.tokens(&sentence).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        for (group, tokens) in self.groups.iter_mut().zip(group_tokens.iter()) {
            if !tokens.is_empty() {
                group.span.set_start(tokens[0].span().start());
                group.span.set_end(tokens[tokens.len() - 1].span().end());
                start = tokens[tokens.len() - 1].span().end();
            } else {
                group.span.set_end(start);
            }
        }

        for (group, tokens) in self.groups.iter_mut().zip(group_tokens.iter()).rev() {
            if !tokens.is_empty() {
                end = tokens[0].span().start();
            } else {
                group.span.set_start(end);
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Part {
    pub atom: Atom,
    pub quantifier: Quantifier,
    pub greedy: bool,
    pub visible: bool,
    pub unify: Option<bool>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Composition {
    pub(crate) parts: Vec<Part>,
    pub(crate) id_to_idx: DefaultHashMap<GraphId, usize>,
    pub(crate) can_stop_mask: Vec<bool>,
}

impl ReadProperties for Composition {
    fn properties(&self) -> Properties {
        self.parts
            .iter()
            .map(|part| part.atom.properties())
            .collect()
    }
}

impl Composition {
    fn next_can_match(
        &self,
        context: Context,
        position: usize,
        index: usize,
    ) -> Result<bool, crate::properties::Error> {
        let next_required_pos = match self.parts[index + 1..]
            .iter()
            .position(|x| x.quantifier.min > 0)
        {
            Some(pos) => index + 1 + pos + 1,
            None => self.parts.len(),
        };

        for part in &self.parts[index + 1..next_required_pos] {
            if part.atom.is_match(context, position)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    fn apply_recursive<'t>(
        &'t self,
        sentence: &'t MatchSentence,
        mut position: usize,
        mut cur_atom_idx: usize,
        mut graph: MatchGraph<'t>,
    ) -> Result<Option<MatchGraph<'t>>, crate::properties::Error> {
        let mut cur_count = 0;
        let is_match = loop {
            if cur_atom_idx >= self.parts.len() {
                break true;
            }

            let part = &self.parts[cur_atom_idx];

            if cur_count >= part.quantifier.max {
                cur_atom_idx += 1;
                cur_count = 0;
                if cur_atom_idx >= self.parts.len() {
                    break false;
                }
                continue;
            }

            if position >= sentence.len() {
                break false;
            }

            if cur_count >= part.quantifier.min && cur_atom_idx + 1 < self.parts.len() {
                if !part.greedy
                    && self.next_can_match((sentence, &graph), position, cur_atom_idx)?
                {
                    cur_atom_idx += 1;
                    cur_count = 0;
                    continue;
                }
                if part.greedy {
                    if let Some(graph) =
                        self.apply_recursive(sentence, position, cur_atom_idx + 1, graph.clone())?
                    {
                        return Ok(Some(graph));
                    }
                }
            }

            if part.atom.is_match((sentence, &graph), position)? {
                let group = &mut graph.groups[cur_atom_idx + 1];

                // set the group beginning if the char end was zero (i. e. the group was empty)
                if group.span.char().end == 0 {
                    group
                        .span
                        .set_start(sentence.index(position).span().start());
                }
                group.span.set_end(sentence.index(position).span().end());

                position += 1;
                cur_count += 1;
            } else {
                break false;
            }
        };

        // edge case if the last atom is quantified and the minimum has been exceeded
        // TODO to avoid further such undetected edge cases: revisit the entire composition logic
        if cur_atom_idx < self.parts.len() && cur_count >= self.parts[cur_atom_idx].quantifier.min {
            cur_atom_idx += 1;
        }

        Ok(
            if is_match || cur_atom_idx == self.parts.len() || self.can_stop_mask[cur_atom_idx] {
                graph.fill_empty(sentence);
                Some(graph)
            } else {
                None
            },
        )
    }

    pub fn apply<'t>(
        &'t self,
        sentence: &'t MatchSentence,
        start: usize,
    ) -> Result<Option<MatchGraph<'t>>, crate::properties::Error> {
        // this path is extremely hot so more optimizations are done

        // the first matcher can never rely on the match graph, so we use an empty default graph for the first match
        // then allocate a new graph if the first matcher matched
        lazy_static! {
            static ref DEFAULT_GRAPH: MatchGraph<'static> = MatchGraph::default();
        };

        if self.parts[0].quantifier.min > 0
            && !self.parts[0]
                .atom
                .is_match((sentence, &DEFAULT_GRAPH), start)?
        {
            return Ok(None);
        }

        let position = start;

        let graph = MatchGraph::new(
            vec![Group::default(); self.parts.len() + 1],
            &self.id_to_idx,
        );

        self.apply_recursive(sentence, position, 0, graph)
    }
}
