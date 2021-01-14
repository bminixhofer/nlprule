use crate::{
    tokenizer::tag::Tagger,
    types::{Token, WordId},
    utils::{parallelism::MaybeParallelIterator, regex::SerializeRegex},
};
use enum_dispatch::enum_dispatch;
use fnv::FnvHashMap;
use fnv::FnvHashSet;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use unicase::UniCase;
#[derive(Debug, Serialize, Deserialize)]
pub struct Matcher {
    matcher: either::Either<either::Either<String, usize>, SerializeRegex>,
    negate: bool,
    case_sensitive: bool,
    empty_always_false: bool,
}

impl Matcher {
    pub fn new_regex(regex: SerializeRegex, negate: bool, empty_always_false: bool) -> Self {
        Matcher {
            matcher: either::Right(regex),
            negate,
            case_sensitive: true, // handled by regex
            empty_always_false,
        }
    }

    pub fn new_string(
        string_or_idx: either::Either<String, usize>,
        negate: bool,
        case_sensitive: bool,
        empty_always_false: bool,
    ) -> Self {
        Matcher {
            matcher: either::Left(string_or_idx),
            negate,
            case_sensitive,
            empty_always_false,
        }
    }

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

    fn needs_graph(&self) -> bool {
        matches!(&self.matcher, either::Left(either::Right(_)))
    }

    fn is_regex(&self) -> bool {
        matches!(&self.matcher, either::Right(_))
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
                either::Right(idx) => graph.by_id(*idx).map_or(false, |x| {
                    x.tokens(&graph.tokens).get(0).map_or(false, |token| {
                        if case_sensitive {
                            token.word.text.as_ref() == input
                        } else {
                            UniCase::new(token.word.text.as_ref()) == UniCase::new(input)
                        }
                    })
                }),
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
pub struct TextMatcher {
    matcher: Matcher,
    set: Option<FnvHashSet<u32>>,
}

impl TextMatcher {
    pub fn new(matcher: Matcher, tagger: &Tagger) -> Self {
        let graph = MatchGraph::default();

        let set = if matcher.needs_graph() || !matcher.is_regex() {
            None
        } else {
            let data: Vec<_> = tagger.word_store().iter().collect();
            let set: FnvHashSet<u32> = data
                .into_maybe_par_iter()
                .filter_map(|(word, id)| {
                    if matcher.is_match(word.as_str(), &graph, None) {
                        Some(*id)
                    } else {
                        None
                    }
                })
                .collect();

            // there are some regexes which match lots of strings
            // this cutoff is pretty arbitrary but without any threshold the size of some sets blows up
            // the vast majority of regexes matches less than 100 strings from manual inspection
            if set.len() > 100 {
                None
            } else {
                Some(set)
            }
        };

        TextMatcher { matcher, set }
    }

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

#[derive(Debug, Serialize, Deserialize)]
pub struct PosMatcher {
    mask: Vec<bool>,
}

impl PosMatcher {
    pub fn new(matcher: Matcher, tagger: &Tagger) -> Self {
        let mut mask = vec![false; tagger.tag_store().len()];
        let graph = MatchGraph::default();

        for (word, id) in tagger.tag_store().iter() {
            mask[*id as usize] = matcher.is_match(word.as_str(), &graph, None);
        }

        PosMatcher { mask }
    }

    pub fn is_match(&self, pos_id: u16) -> bool {
        self.mask[pos_id as usize]
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WordDataMatcher {
    pos_matcher: Option<PosMatcher>,
    inflect_matcher: Option<TextMatcher>,
}

impl WordDataMatcher {
    pub fn new(pos_matcher: Option<PosMatcher>, inflect_matcher: Option<TextMatcher>) -> Self {
        WordDataMatcher {
            pos_matcher,
            inflect_matcher,
        }
    }

    pub fn is_match(
        &self,
        input: &[(u16, &WordId)],
        graph: &MatchGraph,
        case_sensitive: Option<bool>,
    ) -> bool {
        input.iter().any(|x| {
            let pos_matches = self.pos_matcher.as_ref().map_or(true, |m| m.is_match(x.0));

            // matching part-of-speech tag is faster than inflection, so check POS first and early exit if it doesn't match
            if !pos_matches {
                return false;
            }

            let inflect_matches = self
                .inflect_matcher
                .as_ref()
                .map_or(true, |m| m.is_match(x.1, graph, case_sensitive));

            inflect_matches
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Quantifier {
    pub min: usize,
    pub max: usize,
}

impl Quantifier {
    pub fn new(min: usize, max: usize) -> Self {
        assert!(max >= min);
        Quantifier { min, max }
    }
}

#[enum_dispatch]
pub trait Atomable: Send + Sync {
    fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool;
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
    use super::{Atomable, MatchGraph, Matcher, TextMatcher, Token, WordDataMatcher};
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize)]
    pub struct TextAtom {
        matcher: TextMatcher,
    }

    impl Atomable for TextAtom {
        fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
            self.matcher
                .is_match(&input[position].word.text, graph, None)
        }
    }

    impl TextAtom {
        pub fn new(matcher: TextMatcher) -> Self {
            TextAtom { matcher }
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct ChunkAtom {
        matcher: Matcher,
    }

    impl Atomable for ChunkAtom {
        fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
            self.matcher
                .is_slice_match(&input[position].chunks, graph, None)
        }
    }

    impl ChunkAtom {
        pub fn new(matcher: Matcher) -> Self {
            ChunkAtom { matcher }
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct SpaceBeforeAtom {
        value: bool,
    }

    impl Atomable for SpaceBeforeAtom {
        fn is_match(&self, input: &[&Token], _graph: &MatchGraph, position: usize) -> bool {
            input[position].has_space_before == self.value
        }
    }

    impl SpaceBeforeAtom {
        pub fn new(value: bool) -> Self {
            SpaceBeforeAtom { value }
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct WordDataAtom {
        matcher: WordDataMatcher,
        case_sensitive: bool,
    }

    impl Atomable for WordDataAtom {
        fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
            let tags = &input[position].word.tags;

            self.matcher.is_match(
                &tags
                    .iter()
                    .map(|x| (x.pos_id, &x.lemma))
                    .collect::<Vec<_>>(),
                graph,
                Some(self.case_sensitive),
            )
        }
    }

    impl WordDataAtom {
        pub fn new(matcher: WordDataMatcher, case_sensitive: bool) -> Self {
            WordDataAtom {
                matcher,
                case_sensitive,
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TrueAtom {}

impl Atomable for TrueAtom {
    fn is_match(&self, _input: &[&Token], _graph: &MatchGraph, _position: usize) -> bool {
        true
    }
}

impl TrueAtom {
    pub fn new() -> Self {
        TrueAtom {}
    }
}

impl Default for TrueAtom {
    fn default() -> Self {
        TrueAtom::new()
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FalseAtom {}

impl Atomable for FalseAtom {
    fn is_match(&self, _input: &[&Token], _graph: &MatchGraph, _position: usize) -> bool {
        false
    }
}

impl FalseAtom {
    pub fn new() -> Self {
        FalseAtom {}
    }
}

impl Default for FalseAtom {
    fn default() -> Self {
        FalseAtom::new()
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AndAtom {
    atoms: Vec<Atom>,
}

impl AndAtom {
    pub fn and(atoms: Vec<Atom>) -> Atom {
        let mut atoms: Vec<_> = atoms
            .into_iter()
            .filter(|x| !matches!(x, Atom::TrueAtom { .. }))
            .collect();

        if atoms.is_empty() {
            (TrueAtom {}).into()
        } else if atoms.len() == 1 {
            atoms.remove(0)
        } else {
            (AndAtom { atoms }).into()
        }
    }
}

impl Atomable for AndAtom {
    fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
        self.atoms
            .iter()
            .all(|x| x.is_match(input, graph, position))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OrAtom {
    atoms: Vec<Atom>,
}

impl OrAtom {
    pub fn or(atoms: Vec<Atom>) -> Atom {
        let mut atoms: Vec<_> = atoms
            .into_iter()
            .filter(|x| !matches!(x, Atom::FalseAtom { .. }))
            .collect();

        if atoms.is_empty() {
            (FalseAtom {}).into()
        } else if atoms.len() == 1 {
            atoms.remove(0)
        } else {
            (OrAtom { atoms }).into()
        }
    }
}

impl Atomable for OrAtom {
    fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
        self.atoms
            .iter()
            .any(|x| x.is_match(input, graph, position))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NotAtom {
    atom: Box<Atom>,
}

impl NotAtom {
    pub fn not(atom: Atom) -> Atom {
        match atom {
            Atom::TrueAtom { .. } => FalseAtom::new().into(),
            Atom::FalseAtom { .. } => TrueAtom::new().into(),
            x => (NotAtom { atom: Box::new(x) }).into(),
        }
    }
}

impl Atomable for NotAtom {
    fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
        !self.atom.is_match(input, graph, position)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OffsetAtom {
    atom: Box<Atom>,
    offset: isize,
}

impl Atomable for OffsetAtom {
    fn is_match(&self, input: &[&Token], graph: &MatchGraph, position: usize) -> bool {
        let new_position = position as isize + self.offset;

        if new_position < 0 || (new_position as usize) >= input.len() {
            false
        } else {
            self.atom.is_match(input, graph, new_position as usize)
        }
    }
}

impl OffsetAtom {
    pub fn new(atom: Atom, offset: isize) -> Self {
        OffsetAtom {
            atom: Box::new(atom),
            offset,
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

    pub fn tokens<'t>(&self, tokens: &[&'t Token<'t>]) -> Vec<&'t Token<'t>> {
        tokens
            .iter()
            .filter_map(|x| {
                if x.char_span.1 > x.char_span.0 // special tokens with zero range (e. g. SENT_START) can not be part of groups
                    && x.char_span.0 >= self.char_span.0
                    && x.char_span.1 <= self.char_span.1
                {
                    Some(*x)
                } else {
                    None
                }
            })
            .collect()
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

#[derive(Debug)]
pub struct MatchGraph<'t> {
    groups: Vec<Group>,
    id_to_idx: &'t FnvHashMap<usize, usize>,
    tokens: Vec<&'t Token<'t>>,
}

lazy_static! {
    static ref EMPTY_MAP: FnvHashMap<usize, usize> = FnvHashMap::default();
}

impl<'t> Default for MatchGraph<'t> {
    fn default() -> Self {
        MatchGraph {
            groups: Vec::new(),
            id_to_idx: &(*EMPTY_MAP),
            tokens: Vec::new(),
        }
    }
}

impl<'t> MatchGraph<'t> {
    pub fn new(
        groups: Vec<Group>,
        id_to_idx: &'t FnvHashMap<usize, usize>,
        tokens: Vec<&'t Token<'t>>,
    ) -> Self {
        MatchGraph {
            groups,
            id_to_idx,
            tokens,
        }
    }

    pub fn by_index(&self, index: usize) -> &Group {
        &self.groups[index]
    }

    pub fn by_id(&self, id: usize) -> Option<&Group> {
        Some(&self.groups[self.get_index(id)?])
    }

    pub fn get_index(&self, id: usize) -> Option<usize> {
        Some(*self.id_to_idx.get(&id)?)
    }

    pub fn groups(&self) -> &[Group] {
        &self.groups[..]
    }

    pub fn tokens(&self) -> &[&'t Token<'t>] {
        &self.tokens[..]
    }

    pub fn fill_empty(&mut self) {
        let mut start = self
            .groups
            .iter()
            .find_map(|x| {
                let tokens = x.tokens(&self.tokens);
                if tokens.is_empty() {
                    None
                } else {
                    Some(tokens[0].char_span.0)
                }
            })
            .expect("graph must contain at least one token");

        let mut end = self
            .groups
            .iter()
            .rev()
            .find_map(|x| {
                let tokens = x.tokens(&self.tokens);
                if tokens.is_empty() {
                    None
                } else {
                    Some(tokens[tokens.len() - 1].char_span.1)
                }
            })
            .expect("graph must contain at least one token");

        let group_tokens: Vec<_> = self
            .groups
            .iter()
            .map(|x| x.tokens(&self.tokens))
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

#[derive(Serialize, Deserialize)]
pub struct Part {
    pub atom: Atom,
    pub quantifier: Quantifier,
    pub visible: bool,
}

impl Part {
    pub fn new(atom: Atom, quantifier: Quantifier, visible: bool) -> Self {
        Part {
            atom,
            quantifier,
            visible,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Composition {
    pub parts: Vec<Part>,
    group_ids_to_idx: FnvHashMap<usize, usize>,
    can_stop_mask: Vec<bool>,
}

impl Composition {
    pub fn new(parts: Vec<Part>) -> Self {
        let mut group_ids_to_idx = FnvHashMap::default();
        group_ids_to_idx.insert(0, 0);
        let mut current_id = 1;

        for (i, part) in parts.iter().enumerate() {
            if part.visible {
                group_ids_to_idx.insert(current_id, i + 1);
                current_id += 1;
            }
        }

        let can_stop_mask = (0..parts.len())
            .map(|i| parts[i..].iter().all(|x| x.quantifier.min == 0))
            .collect();

        Composition {
            parts,
            group_ids_to_idx,
            can_stop_mask,
        }
    }

    fn next_can_match(
        &self,
        tokens: &[&Token],
        graph: &MatchGraph,
        position: usize,
        index: usize,
    ) -> bool {
        if index == self.parts.len() - 1 {
            return false;
        }

        let next_required_pos = match self.parts[index + 1..]
            .iter()
            .position(|x| x.quantifier.min > 0)
        {
            Some(pos) => index + 1 + pos + 1,
            None => self.parts.len(),
        };

        self.parts[index + 1..next_required_pos]
            .iter()
            .any(|x| x.atom.is_match(tokens, graph, position))
    }

    pub fn apply<'t>(&'t self, tokens: &[&'t Token<'t>], start: usize) -> Option<MatchGraph<'t>> {
        // this path is extremely hot so more optimizations are done

        // the first matcher can never rely on the match graph, so we use an empty default graph for the first match
        // then allocate a new graph if the first matcher matched
        lazy_static! {
            static ref DEFAULT_GRAPH: MatchGraph<'static> = MatchGraph::default();
        };

        let first_must_match = self.parts[0].quantifier.min > 0;
        if first_must_match && !self.parts[0].atom.is_match(tokens, &DEFAULT_GRAPH, start) {
            return None;
        }

        let mut position = start;

        let mut cur_count = 0;
        let mut cur_atom_idx = 0;

        let mut graph = MatchGraph::new(
            vec![Group::default(); self.parts.len() + 1],
            &self.group_ids_to_idx,
            tokens.to_vec(),
        );

        let mut is_match = loop {
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

            if position >= tokens.len() {
                break false;
            }

            if cur_count >= part.quantifier.min
                && self.next_can_match(&tokens, &graph, position, cur_atom_idx)
            {
                cur_atom_idx += 1;
                cur_count = 0;
            } else if (first_must_match && position == start && cur_atom_idx == 0) // we already know this must have matched, otherwise it would have early exited above
                || part.atom.is_match(tokens, &graph, position)
            {
                let mut group = &mut graph.groups[cur_atom_idx + 1];

                // set the group beginning if the char end was zero (i. e. the group was empty)
                if group.char_span.1 == 0 {
                    group.char_span.0 = tokens[position].char_span.0;
                }
                group.char_span.1 = tokens[position].char_span.1;

                position += 1;
                cur_count += 1;
            } else {
                break false;
            }
        };

        is_match = is_match || cur_atom_idx == self.parts.len() || self.can_stop_mask[cur_atom_idx];

        if is_match {
            graph.fill_empty();
            Some(graph)
        } else {
            None
        }
    }
}
