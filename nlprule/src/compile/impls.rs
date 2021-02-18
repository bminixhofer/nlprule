use std::{
    collections::HashMap,
    fs::File,
    hash::{Hash, Hasher},
    io::{self, BufRead, BufReader},
    path::Path,
    sync::Arc,
};

use log::warn;
use serde::{Deserialize, Serialize};

use crate::{
    rule::{
        disambiguation::POSFilter,
        engine::{
            composition::{GraphId, Matcher, PosMatcher, TextMatcher},
            Engine,
        },
        DisambiguationRule, MatchGraph, Rule,
    },
    rules::{Rules, RulesOptions},
    tokenizer::{
        chunk,
        multiword::{MultiwordTagger, MultiwordTaggerFields},
        Tokenizer, TokenizerOptions,
    },
    types::*,
    utils::{parallelism::MaybeParallelIterator, regex::Regex},
};

use super::{parse_structure::BuildInfo, Error};

impl MultiwordTagger {
    pub fn from_dump<P: AsRef<Path>>(dump: P, info: &BuildInfo) -> Result<Self, io::Error> {
        let reader = BufReader::new(File::open(dump)?);
        let mut multiwords = Vec::new();

        for line in reader.lines() {
            let line = line?;

            // strip comments
            let line = &line[..line.find('#').unwrap_or_else(|| line.len())].trim();
            if line.is_empty() {
                continue;
            }
            let tab_split: Vec<_> = line.split('\t').collect();

            let word: String = tab_split[0]
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ");
            let pos = info.tagger().id_tag(tab_split[1]).to_owned_id();
            multiwords.push((word, pos));
        }

        Ok((MultiwordTaggerFields { multiwords }).into())
    }
}

impl TextMatcher {
    pub fn new(matcher: Matcher, info: &mut BuildInfo) -> Self {
        let graph = MatchGraph::default();

        // can not cache a matcher that depends on the graph
        let set = if matcher.graph_id().is_some() {
            None
        } else if let either::Right(regex) = &matcher.matcher {
            let mut hasher = DefaultHasher::default();
            regex.hash(&mut hasher);
            matcher.negate.hash(&mut hasher);
            matcher.empty_always_false.hash(&mut hasher);
            let matcher_hash = hasher.finish();

            if let Some(set) = info.mut_regex_cache().get(&matcher_hash) {
                set.clone()
            } else {
                let data: Vec<_> = info.tagger().word_store().iter().collect();

                let set: DefaultHashSet<_> = data
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
                let set = if set.len() > 100 { None } else { Some(set) };
                info.mut_regex_cache().insert(matcher_hash, set.clone());
                set
            }
        } else {
            None
        };

        TextMatcher { matcher, set }
    }
}

impl PosMatcher {
    pub fn new(matcher: Matcher, info: &mut BuildInfo) -> Self {
        let mut mask = vec![false; info.tagger().tag_store().len()];
        let graph = MatchGraph::default();

        for (word, id) in info.tagger().tag_store().iter() {
            mask[id.0 as usize] = matcher.is_match(word.as_str(), &graph, None);
        }

        PosMatcher { mask }
    }
}

impl Rules {
    pub fn from_xml<P: AsRef<Path>>(
        path: P,
        build_info: &mut BuildInfo,
        options: &RulesOptions,
    ) -> Self {
        let rules = super::parse_structure::read_rules(path);
        let mut errors: HashMap<String, usize> = HashMap::new();

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, group, category)) => {
                    let id = rule_structure.id.as_ref().map_or_else(
                        || {
                            let group = group.as_ref().expect("must have group if ID not set");
                            format!("{}.{}", group.id, group.n)
                        },
                        |x| x.clone(),
                    );
                    let category = category.expect("grammar rules must have category");
                    let off = rule_structure
                        .default
                        .as_ref()
                        .map(|x| x == "off")
                        .or_else(|| {
                            group
                                .as_ref()
                                .and_then(|x| x.default.as_ref().map(|x| x == "off"))
                        })
                        .or_else(|| category.default.as_ref().map(|x| x == "off"))
                        .unwrap_or(false);
                    let name = rule_structure.name.as_ref().map_or_else(
                        || {
                            let group = group.as_ref().expect("must have group if name not set");
                            group.name.clone()
                        },
                        |x| x.clone(),
                    );

                    match Rule::from_rule_structure(rule_structure, build_info) {
                        Ok(mut rule) => {
                            if (options.ids.is_empty() || options.ids.contains(&id))
                                && !options.ignore_ids.contains(&id)
                            {
                                rule.id = id;
                                rule.name = name;
                                rule.on = !off;
                                rule.category_id = category.id;
                                rule.category_name = category.name;
                                rule.category_type = category.kind;
                                Some(rule)
                            } else {
                                None
                            }
                        }
                        Err(x) => {
                            *errors.entry(format!("[Rule] {}", x)).or_insert(0) += 1;
                            None
                        }
                    }
                }
                Err(x) => {
                    *errors.entry(format!("[Structure] {}", x)).or_insert(0) += 1;
                    None
                }
            })
            .collect();

        if !errors.is_empty() {
            let mut errors: Vec<(String, usize)> = errors.into_iter().collect();
            errors.sort_by_key(|x| -(x.1 as i32));

            warn!(
                "Errors constructing Rules: {:#?}",
                &errors
                    .iter()
                    .map(|(message, number)| format!("{} (n={})", message, number))
                    .collect::<Vec<_>>()
            );
        }

        Rules { rules }
    }
}

impl Tokenizer {
    pub fn from_xml<P: AsRef<Path>>(
        path: P,
        build_info: &mut BuildInfo,
        chunker: Option<chunk::Chunker>,
        multiword_tagger: Option<MultiwordTagger>,
        sentencizer: srx::Rules,
        options: Arc<TokenizerOptions>,
    ) -> Result<Self, Error> {
        let rules = super::parse_structure::read_disambiguation_rules(path);
        let mut error = None;

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, group, _)) => {
                    let id = rule_structure.id.as_ref().map_or_else(
                        || {
                            let group = group.expect("must have group if ID not set");
                            format!("{}.{}", group.id, group.n)
                        },
                        |x| x.clone(),
                    );

                    match DisambiguationRule::from_rule_structure(rule_structure, build_info) {
                        Ok(mut rule) => {
                            if error.is_none()
                                && (options.ids.is_empty() || options.ids.contains(&id))
                                && !options.ignore_ids.contains(&id)
                            {
                                rule.id = id;

                                Some(rule)
                            } else {
                                None
                            }
                        }
                        Err(x) => {
                            if error.is_none() {
                                error = Some(format!("[Rule] {}", x));
                            }
                            None
                        }
                    }
                }
                Err(x) => {
                    if error.is_none() {
                        error = Some(format!("[Structure] {}", x));
                    }
                    None
                }
            })
            .collect();

        if let Some(x) = error {
            if options.allow_errors {
                warn!("Error constructing Disambiguator: {}", x)
            } else {
                return Err(Error::Unexpected(format!(
                    "Error constructing Disambiguator: {}",
                    x
                )));
            }
        }

        Ok(Tokenizer {
            tagger: build_info.tagger().clone(),
            sentencizer,
            chunker,
            multiword_tagger,
            rules,
            options,
        })
    }
}

#[derive(Deserialize)]
struct ModelData {
    outcome_labels: Vec<String>,
    pmap: DefaultHashMap<String, ContextData>,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct ContextData {
    parameters: Vec<f32>,
    outcomes: Vec<usize>,
}

impl From<ContextData> for chunk::Context {
    fn from(data: ContextData) -> Self {
        chunk::Context {
            parameters: data.parameters,
            outcomes: data.outcomes,
        }
    }
}

impl From<ModelData> for chunk::Model {
    fn from(data: ModelData) -> Self {
        chunk::Model {
            outcome_labels: data.outcome_labels,
            pmap: data
                .pmap
                .into_iter()
                .map(|(key, value)| (chunk::hash::hash_str(&key), value.into()))
                .collect::<DefaultHashMap<_, _>>(),
        }
    }
}

impl chunk::Chunker {
    pub fn from_json<R: std::io::Read>(reader: R) -> Result<chunk::Chunker, serde_json::Error> {
        #[derive(Deserialize)]
        struct ChunkData {
            token_model: ModelData,
            pos_model: ModelData,
            pos_tagdict: DefaultHashMap<String, Vec<String>>,
            chunk_model: ModelData,
        }

        let chunk_data: ChunkData = serde_json::from_reader(reader)?;
        Ok(chunk::Chunker {
            token_model: chunk::MaxentTokenizer {
                model: chunk_data.token_model.into(),
            },
            pos_model: chunk::MaxentPosTagger {
                model: chunk_data.pos_model.into(),
                tagdict: chunk_data.pos_tagdict,
            },
            chunk_model: chunk::MaxentChunker {
                model: chunk_data.chunk_model.into(),
            },
        })
    }
}

impl POSFilter {
    pub fn new(matcher: PosMatcher) -> Self {
        POSFilter { matcher }
    }
}

impl Regex {
    pub fn from_java_regex(
        regex_str: &str,
        full_match: bool,
        case_sensitive: bool,
    ) -> Result<Self, Error> {
        fn unescape<S: AsRef<str>>(string: S, c: &str) -> String {
            let placeholder = "###escaped_backslash###";

            string
                .as_ref()
                .replace(r"\\", placeholder)
                .replace(&format!(r"\{}", c), c)
                .replace(placeholder, r"\\")
        }

        // TODO: more exhaustive backslash check
        let mut fixed = unescape(unescape(unescape(regex_str, "!"), ","), "/");
        let mut case_sensitive = case_sensitive;

        fixed = fixed
            .replace("\\\\s", "###backslash_before_s###")
            .replace("\\$", "###escaped_dollar###")
            // apparently \s in Java regexes only matches an actual space, not e.g non-breaking space
            .replace("\\s", " ")
            .replace("$+", "$")
            .replace("$?", "$")
            .replace("$*", "$")
            .replace("###escaped_dollar###", "\\$")
            .replace("###backslash_before_s###", "\\\\s");

        for pattern in &["(?iu)", "(?i)"] {
            if fixed.contains(pattern) {
                case_sensitive = false;
                fixed = fixed.replace(pattern, "");
            }
        }

        let fixed = if full_match {
            format!("^({})$", fixed)
        } else {
            fixed
        };

        let fixed = if case_sensitive {
            fixed
        } else {
            format!("(?i){}", fixed)
        };

        let regex = Regex::new(fixed);
        regex.try_compile().map_err(Error::Regex)?;
        Ok(regex)
    }
}

impl Engine {
    pub fn to_graph_id(&self, id: usize) -> Result<GraphId, Error> {
        let mut id = GraphId(id);

        let map = match &self {
            Engine::Token(engine) => &engine.composition.id_to_idx,
            Engine::Text(_, id_to_idx) => &id_to_idx,
        };

        let max_id = *map
            .keys()
            .max()
            .ok_or_else(|| Error::Unexpected("graph is empty".into()))?;

        // ideally this should throw an error but LT is more lenient than nlprule
        if !map.contains_key(&id) {
            id = max_id;
        }

        Ok(id)
    }
}

mod composition {
    use super::*;
    use crate::{
        rule::engine::composition::{
            AndAtom, Atom, Composition, FalseAtom, GraphId, NotAtom, OffsetAtom, OrAtom, Part,
            Quantifier, TrueAtom,
        },
        utils::regex::Regex,
    };

    impl Atom {
        fn iter_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut Atom> + 'a> {
            match self {
                Atom::ChunkAtom(_)
                | Atom::SpaceBeforeAtom(_)
                | Atom::TextAtom(_)
                | Atom::WordDataAtom(_)
                | Atom::FalseAtom(_)
                | Atom::TrueAtom(_) => Box::new(std::iter::once(self)),
                Atom::AndAtom(x) => Box::new(x.atoms.iter_mut()),
                Atom::OrAtom(x) => Box::new(x.atoms.iter_mut()),
                Atom::NotAtom(x) => x.atom.iter_mut(),
                Atom::OffsetAtom(x) => x.atom.iter_mut(),
            }
        }

        pub fn mut_graph_ids(&mut self) -> Vec<&mut GraphId> {
            let mut ids = Vec::new();

            for atom in self.iter_mut() {
                let id = match atom {
                    Atom::ChunkAtom(atom) => atom.matcher.mut_graph_id(),
                    Atom::TextAtom(atom) => atom.matcher.matcher.mut_graph_id(),
                    Atom::WordDataAtom(atom) => atom
                        .matcher
                        .inflect_matcher
                        .as_mut()
                        .and_then(|x| x.matcher.mut_graph_id()),
                    _ => {
                        continue;
                    }
                };

                if let Some(id) = id {
                    ids.push(id);
                }
            }

            ids
        }
    }

    impl Matcher {
        pub fn new_regex(regex: Regex, negate: bool, empty_always_false: bool) -> Self {
            Matcher {
                matcher: either::Right(regex),
                negate,
                case_sensitive: true, // handled by regex, should maybe be an option
                empty_always_false,
            }
        }

        pub fn new_string(
            string_or_idx: either::Either<String, GraphId>,
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

        pub fn graph_id(&self) -> Option<GraphId> {
            if let either::Left(either::Right(id)) = &self.matcher {
                Some(*id)
            } else {
                None
            }
        }

        pub fn mut_graph_id(&mut self) -> Option<&mut GraphId> {
            if let either::Left(either::Right(id)) = &mut self.matcher {
                Some(id)
            } else {
                None
            }
        }
    }

    impl Quantifier {
        pub fn new(min: usize, max: usize) -> Self {
            assert!(max >= min);
            Quantifier { min, max }
        }
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

    impl NotAtom {
        pub fn not(atom: Atom) -> Atom {
            match atom {
                Atom::TrueAtom { .. } => FalseAtom::default().into(),
                Atom::FalseAtom { .. } => TrueAtom::default().into(),
                x => (NotAtom { atom: Box::new(x) }).into(),
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

    impl Composition {
        pub fn new(mut parts: Vec<Part>) -> Result<Self, Error> {
            let mut id_to_idx = DefaultHashMap::default();
            id_to_idx.insert(GraphId(0), 0);
            let mut current_id = 1;

            for (i, part) in parts.iter().enumerate() {
                if part.visible {
                    id_to_idx.insert(GraphId(current_id), i + 1);
                    current_id += 1;
                }
            }

            let can_stop_mask = (0..parts.len())
                .map(|i| parts[i..].iter().all(|x| x.quantifier.min == 0))
                .collect();

            for (i, part) in parts.iter_mut().enumerate() {
                for id in part.atom.mut_graph_ids() {
                    loop {
                        let index = *id_to_idx.get(&id).ok_or_else(|| {
                            Error::Unexpected(format!("id must exist in graph: {:?}", id))
                        })?;

                        // ideally this should throw an error but LT is more lenient than nlprule
                        if index > i {
                            *id = GraphId(id.0 - 1);
                        } else {
                            break;
                        }
                    }
                }
            }

            Ok(Composition {
                parts,
                id_to_idx,
                can_stop_mask,
            })
        }
    }
}

pub mod filters {
    use super::Error;
    use std::collections::HashMap;

    use crate::{filter::*, rule::engine::Engine, utils::regex::Regex};

    trait FromArgs: Sized {
        fn from_args(args: HashMap<String, String>, engine: &Engine) -> Result<Self, Error>;
    }

    impl FromArgs for NoDisambiguationEnglishPartialPosTagFilter {
        fn from_args(args: HashMap<String, String>, engine: &Engine) -> Result<Self, Error> {
            if args.contains_key("negate_postag") {
                panic!("negate_postag not supported in NoDisambiguationEnglishPartialPosTagFilter");
            }

            Ok(NoDisambiguationEnglishPartialPosTagFilter {
                id: engine.to_graph_id(args
                    .get("no")
                    .ok_or_else(|| {
                        Error::Unexpected(
                            "NoDisambiguationEnglishPartialPosTagFilter must have `no` argument"
                                .into(),
                        )
                    })?
                    .parse::<usize>()?)?,
                regexp: Regex::from_java_regex(
                    &args.get("regexp").ok_or_else(|| {
                        Error::Unexpected(
                        "NoDisambiguationEnglishPartialPosTagFilter must have `regexp` argument"
                            .into(),
                    )
                    })?,
                    true,
                    true,
                )?,
                postag_regexp: Regex::from_java_regex(
                    &args.get("postag_regexp").ok_or_else(|| {
                        Error::Unexpected(
                        "NoDisambiguationEnglishPartialPosTagFilter must have `postag_regexp` argument"
                            .into(),
                    )
                    })?,
                    true,
                    true,
                )?,
                negate_postag: args.get("negate_postag").map_or(false, |x| x == "yes"),
            })
        }
    }

    pub fn get_filter(
        name: &str,
        args: HashMap<String, String>,
        engine: &Engine,
    ) -> Result<Filter, Error> {
        match name {
            "NoDisambiguationEnglishPartialPosTagFilter" => {
                Ok(NoDisambiguationEnglishPartialPosTagFilter::from_args(args, engine)?.into())
            }
            _ => Err(Error::Unexpected(format!("unsupported filter {}", name))),
        }
    }
}
