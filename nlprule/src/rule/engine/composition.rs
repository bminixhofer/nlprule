use crate::{types::*, utils::regex::Regex};
use enum_dispatch::enum_dispatch;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use unicase::UniCase;

#[derive(Debug, Serialize, Deserialize)]
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
        graph: &MatchGraph,
        case_sensitive: Option<bool>,
    ) -> bool {
        input
            .iter()
            .any(|x| self.is_match(x.as_ref(), graph, case_sensitive))
    }

    pub fn is_match(&self, input: &str, graph: &MatchGraph, case_sensitive: Option<bool>) -> bool {
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
                    graph
                        .by_id(*id)
                        .tokens(&graph.sentence)
                        .next()
                        .map_or(false, |token| {
                            if case_sensitive {
                                token.word.text.as_ref() == input
                            } else {
                                UniCase::new(token.word.text.as_ref()) == UniCase::new(input)
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

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct TextMatcher {
    pub(crate) matcher: Matcher,
    pub(crate) set: Option<DefaultHashSet<WordIdInt>>,
}

impl TextMatcher {
    pub fn is_match(
        &self,
        word_id: &WordId,
        graph: &MatchGraph,
        case_sensitive: Option<bool>,
    ) -> bool {
        if self.set.is_none() {
            return self
                .matcher
                .is_match(word_id.as_ref(), graph, case_sensitive);
        }

        if let Some(id) = word_id.id() {
            self.set.as_ref().unwrap().contains(id)
        } else {
            self.matcher
                .is_match(word_id.as_ref(), graph, case_sensitive)
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct PosMatcher {
    pub mask: Vec<bool>,
}

impl PosMatcher {
    pub fn is_match(&self, pos: &PosId) -> bool {
        self.mask[pos.id().0 as usize]
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WordDataMatcher {
    pub(crate) pos_matcher: Option<PosMatcher>,
    pub(crate) inflect_matcher: Option<TextMatcher>,
}

impl WordDataMatcher {
    pub fn is_match(
        &self,
        input: &[WordData],
        graph: &MatchGraph,
        case_sensitive: Option<bool>,
    ) -> bool {
        input.iter().any(|x| {
            let pos_matches = self
                .pos_matcher
                .as_ref()
                .map_or(true, |m| m.is_match(&x.pos));

            // matching part-of-speech tag is faster than inflection, so check POS first and early exit if it doesn't match
            if !pos_matches {
                return false;
            }

            let inflect_matches = self
                .inflect_matcher
                .as_ref()
                .map_or(true, |m| m.is_match(&x.lemma, graph, case_sensitive));

            inflect_matches
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Quantifier {
    pub min: usize,
    pub max: usize,
}

#[enum_dispatch]
pub trait Atomable: Send + Sync {
    fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool;
}

#[enum_dispatch(Atomable)]
#[derive(Debug, Serialize, Deserialize)]
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
    use super::{Atomable, MatchGraph, MatchSentence, Matcher, TextMatcher, WordDataMatcher};
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize)]
    pub struct TextAtom {
        pub(crate) matcher: TextMatcher,
    }

    impl Atomable for TextAtom {
        fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
            self.matcher
                .is_match(&input.index(position).word.text, graph, None)
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct ChunkAtom {
        pub(crate) matcher: Matcher,
    }

    impl Atomable for ChunkAtom {
        fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
            self.matcher
                .is_slice_match(&input.index(position).chunks, graph, None)
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct SpaceBeforeAtom {
        pub(crate) value: bool,
    }

    impl Atomable for SpaceBeforeAtom {
        fn is_match(&self, input: &MatchSentence, _graph: &MatchGraph, position: usize) -> bool {
            input.index(position).has_space_before == self.value
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct WordDataAtom {
        pub(crate) matcher: WordDataMatcher,
        pub(crate) case_sensitive: bool,
    }

    impl Atomable for WordDataAtom {
        fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
            let tags = &input.index(position).word.tags;

            self.matcher
                .is_match(&tags, graph, Some(self.case_sensitive))
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct TrueAtom {}

impl Atomable for TrueAtom {
    fn is_match(&self, _input: &MatchSentence, _graph: &MatchGraph, _position: usize) -> bool {
        true
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct FalseAtom {}

impl Atomable for FalseAtom {
    fn is_match(&self, _input: &MatchSentence, _graph: &MatchGraph, _position: usize) -> bool {
        false
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AndAtom {
    pub(crate) atoms: Vec<Atom>,
}

impl Atomable for AndAtom {
    fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
        self.atoms
            .iter()
            .all(|x| x.is_match(input, graph, position))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OrAtom {
    pub(crate) atoms: Vec<Atom>,
}

impl Atomable for OrAtom {
    fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
        self.atoms
            .iter()
            .any(|x| x.is_match(input, graph, position))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NotAtom {
    pub(crate) atom: Box<Atom>,
}

impl Atomable for NotAtom {
    fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
        !self.atom.is_match(input, graph, position)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OffsetAtom {
    pub(crate) atom: Box<Atom>,
    pub(crate) offset: isize,
}

impl Atomable for OffsetAtom {
    fn is_match(&self, input: &MatchSentence, graph: &MatchGraph, position: usize) -> bool {
        let new_position = position as isize + self.offset;

        if new_position < 0 || (new_position as usize) >= input.len() {
            false
        } else {
            self.atom.is_match(input, graph, new_position as usize)
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Group {
    pub char_span: (usize, usize),
}

impl Group {
    pub fn new(char_span: (usize, usize)) -> Self {
        Group { char_span }
    }

    pub fn tokens<'a, 't>(
        &'a self,
        sentence: &'t MatchSentence,
    ) -> impl DoubleEndedIterator<Item = &'t Token<'t>> {
        let start = self.char_span.0;
        let end = self.char_span.1;

        sentence
            .iter()
            .filter(move |x| x.char_span.0 >= start && x.char_span.1 <= end)
    }

    pub fn text<'a>(&self, text: &'a str) -> &'a str {
        if self.char_span.0 >= self.char_span.1 {
            return "";
        }

        let mut char_indices: Vec<_> = text.char_indices().map(|(i, _)| i).collect();
        char_indices.push(text.len());

        &text[char_indices[self.char_span.0]..char_indices[self.char_span.1]]
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
pub struct MatchGraph<'t> {
    groups: Vec<Group>,
    id_to_idx: &'t DefaultHashMap<GraphId, usize>,
    sentence: &'t MatchSentence<'t>,
}

lazy_static! {
    static ref EMPTY_MAP: DefaultHashMap<GraphId, usize> = DefaultHashMap::default();
}

impl<'t> Default for MatchGraph<'t> {
    fn default() -> Self {
        unimplemented!()
    }
}

impl<'t> MatchGraph<'t> {
    pub fn new(
        groups: Vec<Group>,
        id_to_idx: &'t DefaultHashMap<GraphId, usize>,
        sentence: &'t MatchSentence,
    ) -> Self {
        MatchGraph {
            groups,
            id_to_idx,
            sentence,
        }
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

    pub fn sentence(&self) -> &MatchSentence<'t> {
        &self.sentence
    }

    pub fn fill_empty(&mut self) {
        let mut start = self
            .groups
            .iter()
            .find_map(|x| {
                x.tokens(&self.sentence)
                    .next()
                    .map(|token| token.char_span.0)
            })
            .expect("graph must contain at least one token");

        let mut end = self
            .groups
            .iter()
            .rev()
            .find_map(|x| {
                x.tokens(&self.sentence)
                    .next_back()
                    .map(|token| token.char_span.1)
            })
            .expect("graph must contain at least one token");

        let group_tokens: Vec<_> = self
            .groups
            .iter()
            .map(|x| x.tokens(&self.sentence).collect::<Vec<_>>())
            .collect::<Vec<_>>();

        for (group, tokens) in self.groups.iter_mut().zip(group_tokens.iter()) {
            if !tokens.is_empty() {
                group.char_span.0 = tokens[0].char_span.0;
                group.char_span.1 = tokens[tokens.len() - 1].char_span.1;
                start = tokens[tokens.len() - 1].char_span.1;
            } else {
                group.char_span.1 = start;
            }
        }

        for (group, tokens) in self.groups.iter_mut().zip(group_tokens.iter()).rev() {
            if !tokens.is_empty() {
                end = tokens[0].char_span.0;
            } else {
                group.char_span.0 = end;
            }
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Part {
    pub atom: Atom,
    pub quantifier: Quantifier,
    pub greedy: bool,
    pub visible: bool,
    pub unify: Option<bool>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Composition {
    pub(crate) parts: Vec<Part>,
    pub(crate) id_to_idx: DefaultHashMap<GraphId, usize>,
    pub(crate) can_stop_mask: Vec<bool>,
}

impl Composition {
    fn next_can_match(
        &self,
        sentence: &MatchSentence,
        graph: &MatchGraph,
        position: usize,
        index: usize,
    ) -> bool {
        let next_required_pos = match self.parts[index + 1..]
            .iter()
            .position(|x| x.quantifier.min > 0)
        {
            Some(pos) => index + 1 + pos + 1,
            None => self.parts.len(),
        };

        self.parts[index + 1..next_required_pos]
            .iter()
            .any(|x| x.atom.is_match(sentence, graph, position))
    }

    fn apply_recursive<'t>(
        &'t self,
        sentence: &'t MatchSentence,
        mut position: usize,
        mut cur_atom_idx: usize,
        mut graph: MatchGraph<'t>,
    ) -> Option<MatchGraph<'t>> {
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
                if !part.greedy && self.next_can_match(sentence, &graph, position, cur_atom_idx) {
                    cur_atom_idx += 1;
                    cur_count = 0;
                    continue;
                }
                if part.greedy {
                    if let Some(graph) =
                        self.apply_recursive(sentence, position, cur_atom_idx + 1, graph.clone())
                    {
                        return Some(graph);
                    }
                }
            }

            if part.atom.is_match(sentence, &graph, position) {
                let mut group = &mut graph.groups[cur_atom_idx + 1];

                // set the group beginning if the char end was zero (i. e. the group was empty)
                if group.char_span.1 == 0 {
                    group.char_span.0 = sentence.index(position).char_span.0;
                }
                group.char_span.1 = sentence.index(position).char_span.1;

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

        if is_match || cur_atom_idx == self.parts.len() || self.can_stop_mask[cur_atom_idx] {
            graph.fill_empty();
            Some(graph)
        } else {
            None
        }
    }

    pub fn apply<'t>(
        &'t self,
        sentence: &'t MatchSentence,
        start: usize,
    ) -> Option<MatchGraph<'t>> {
        // this path is extremely hot so more optimizations are done

        // the first matcher can never rely on the match graph, so we use an empty default graph for the first match
        // then allocate a new graph if the first matcher matched
        lazy_static! {
            static ref DEFAULT_GRAPH: MatchGraph<'static> = MatchGraph::default();
        };

        if self.parts[0].quantifier.min > 0
            && !self.parts[0].atom.is_match(sentence, &DEFAULT_GRAPH, start)
        {
            return None;
        }

        let position = start;

        let graph = MatchGraph::new(
            vec![Group::default(); self.parts.len() + 1],
            &self.id_to_idx,
            sentence,
        );

        self.apply_recursive(sentence, position, 0, graph)
    }
}
