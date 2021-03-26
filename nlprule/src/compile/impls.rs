use bimap::BiMap;
use fs_err::File;
use indexmap::IndexMap;
use log::warn;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    io::{self, BufRead, BufReader},
    path::Path,
};

use crate::{
    rule::{
        disambiguation::POSFilter,
        engine::{
            composition::{GraphId, Matcher, PosMatcher, TextMatcher},
            Engine,
        },
        id::Category,
        DisambiguationRule, Rule,
    },
    rules::{Rules, RulesLangOptions, RulesOptions},
    tokenizer::{
        chunk,
        multiword::{MultiwordTagger, MultiwordTaggerFields},
        tag::{Tagger, TaggerLangOptions},
        Tokenizer, TokenizerLangOptions,
    },
    types::*,
    utils::{parallelism::MaybeParallelIterator, regex::Regex},
};

use super::{parse_structure::BuildInfo, Error};

impl Tagger {
    fn get_lines<S1: AsRef<Path>, S2: AsRef<Path>>(
        paths: &[S1],
        remove_paths: &[S2],
    ) -> std::io::Result<Vec<(String, String, String)>> {
        let mut output = Vec::new();
        let mut disallowed: Vec<String> = Vec::new();

        for path in remove_paths {
            let file = File::open(path.as_ref())?;
            let reader = std::io::BufReader::new(file);

            for line in reader.lines() {
                let line = line?;
                if line.starts_with('#') {
                    continue;
                }

                disallowed.push(line.to_string());
            }
        }

        for path in paths {
            let file = File::open(path.as_ref())?;
            let reader = std::io::BufReader::new(file);

            for line in reader.lines() {
                let line = line?;
                if line.starts_with('#') {
                    continue;
                }

                if disallowed.contains(&line) {
                    continue;
                }

                let parts: Vec<_> = line.split('\t').collect();

                let word = parts[0].to_string();
                let inflection = parts[1].to_string();
                let tag = parts[2].to_string();

                output.push((word, inflection, tag))
            }
        }

        Ok(output)
    }

    /// Creates a tagger from raw files.
    ///
    /// # Arguments
    /// * `paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be added to the tagger.
    /// * `remove_paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be removed from the tagger if present in the files from `paths`.
    pub(in crate::compile) fn from_dumps<S1: AsRef<Path>, S2: AsRef<Path>>(
        paths: &[S1],
        remove_paths: &[S2],
        common_words: &HashSet<String>,
        lang_options: TaggerLangOptions,
    ) -> std::io::Result<Self> {
        let mut tags = DefaultHashMap::default();
        let mut groups = DefaultHashMap::default();

        let mut tag_store = HashSet::new();
        let mut word_store = HashSet::new();

        // hardcoded special tags
        tag_store.insert("");
        tag_store.insert("SENT_START");
        tag_store.insert("SENT_END");
        tag_store.insert("UNKNOWN");

        // add language specific special tags
        tag_store.extend(lang_options.extra_tags.iter().map(|x| x.as_str()));

        let lines = Tagger::get_lines(paths, remove_paths)?;

        let punct = "!\"#$%&\\'()*+,-./:;<=>?@[\\]^_`{|}~";
        for i in 0..punct.len() {
            word_store.insert(&punct[i..(i + 1)]);
        }

        word_store.extend(common_words.iter().map(|x| x.as_str()));

        for (word, inflection, tag) in lines.iter() {
            word_store.insert(word);
            word_store.insert(inflection);
            tag_store.insert(tag);
        }

        // word store ids should be consistent across runs
        let mut word_store: Vec<_> = word_store.iter().collect();
        word_store.sort();

        //  tag store ids should be consistent across runs
        let mut tag_store: Vec<_> = tag_store.iter().collect();
        tag_store.sort();

        let word_store: BiMap<_, _> = word_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), WordIdInt(i as u32)))
            .collect();
        let tag_store: BiMap<_, _> = tag_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), PosIdInt(i as u16)))
            .collect();

        for (word, inflection, tag) in lines.iter() {
            let word_id = word_store.get_by_left(word).unwrap();
            let inflection_id = word_store.get_by_left(inflection).unwrap();
            let pos_id = tag_store.get_by_left(tag).unwrap();

            let group = groups.entry(*inflection_id).or_insert_with(Vec::new);
            if !group.contains(word_id) {
                group.push(*word_id);
            }

            tags.entry(*word_id)
                .or_insert_with(IndexMap::new)
                .entry(*inflection_id)
                .or_insert_with(Vec::new)
                .push(*pos_id);
        }

        Ok(Tagger {
            tags,
            groups,
            word_store,
            tag_store,
            lang_options,
            ..Default::default()
        })
    }
}

impl MultiwordTagger {
    pub(in crate::compile) fn from_dump<P: AsRef<Path>>(
        dump: P,
        info: &BuildInfo,
    ) -> Result<Self, io::Error> {
        let reader = BufReader::new(File::open(dump.as_ref())?);
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
    pub(in crate::compile) fn new(matcher: Matcher, info: &mut BuildInfo) -> Self {
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
                        if matcher.is_match(word.as_str(), None, None) {
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
    pub(in crate::compile) fn new(matcher: Matcher, info: &mut BuildInfo) -> Self {
        let mut mask = vec![false; info.tagger().tag_store().len()];

        for (word, id) in info.tagger().tag_store().iter() {
            mask[id.0 as usize] = matcher.is_match(word.as_str(), None, None);
        }

        PosMatcher { mask }
    }
}

impl Rules {
    pub(in crate::compile) fn from_xml<P: AsRef<Path>>(
        path: P,
        build_info: &mut BuildInfo,
        options: RulesLangOptions,
    ) -> Self {
        let rules = super::parse_structure::read_rules(path);
        let mut errors: HashMap<String, usize> = HashMap::new();

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, group, category)) => {
                    let category = category.expect("grammar rules must have category");
                    let id = Category::new(category.id.as_str());

                    let id = if let Some(group) = &group {
                        id.join(group.id.as_str()).join(group.n)
                    } else {
                        id.join(
                            rule_structure
                                .id
                                .as_ref()
                                .expect("ID must be set if not in group."),
                        )
                        .join(0)
                    };

                    let rule_on = match rule_structure.default.as_deref() {
                        Some("off") | Some("temp_off") => false,
                        Some("on") | None => true,
                        Some(x) => panic!("unknown `default` value: {}", x),
                    };

                    let group_on = match group.as_ref().and_then(|x| x.default.as_deref()) {
                        Some("off") | Some("temp_off") => false,
                        Some("on") | None => true,
                        Some(x) => panic!("unknown `default` value: {}", x),
                    };

                    let category_on = match category.default.as_deref() {
                        Some("off") | Some("temp_off") => false,
                        Some("on") | None => true,
                        Some(x) => panic!("unknown `default` value: {}", x),
                    };

                    let name = rule_structure.name.as_ref().map_or_else(
                        || {
                            let group = group.as_ref().expect("must have group if name not set");
                            group.name.clone()
                        },
                        |x| x.clone(),
                    );

                    match Rule::from_rule_structure(rule_structure, build_info) {
                        Ok(mut rule) => {
                            if (options.ids.is_empty()
                                || options.ids.iter().any(|x| x.is_match(&id)))
                                && !options.ignore_ids.iter().any(|x| x.is_match(&id))
                            {
                                rule.id = id;
                                rule.name = name;
                                rule.category_name = category.name;
                                rule.category_type = category.kind;
                                rule.enabled = category_on && group_on && rule_on;
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

        Rules {
            rules,
            options: RulesOptions::default(),
        }
    }
}

impl Tokenizer {
    pub(in crate::compile) fn from_xml<P: AsRef<Path>>(
        path: P,
        build_info: &mut BuildInfo,
        chunker: Option<chunk::Chunker>,
        multiword_tagger: Option<MultiwordTagger>,
        sentencizer: srx::Rules,
        lang_options: TokenizerLangOptions,
    ) -> Result<Self, Error> {
        let rules = super::parse_structure::read_disambiguation_rules(path);
        let mut error = None;

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, group, _)) => {
                    let id = Category::new("DISAMBIGUATION");

                    let id = if let Some(group) = &group {
                        id.join(group.id.as_str()).join(group.n)
                    } else {
                        id.join(
                            rule_structure
                                .id
                                .as_ref()
                                .expect("ID must be set if not in group."),
                        )
                        .join(0)
                    };

                    match DisambiguationRule::from_rule_structure(rule_structure, build_info) {
                        Ok(mut rule) => {
                            if error.is_none()
                                && (lang_options.ids.is_empty()
                                    || lang_options.ids.iter().any(|x| x.is_match(&id)))
                                && !lang_options.ignore_ids.iter().any(|x| x.is_match(&id))
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
            if lang_options.allow_errors {
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
            lang_options,
        })
    }
}

#[derive(Deserialize)]
struct ModelData {
    outcome_labels: Vec<String>,
    pmap: DefaultHashMap<String, ContextData>,
}

#[derive(Serialize, Deserialize)]
pub(in crate::compile) struct ContextData {
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
    pub(in crate::compile) fn from_json<R: std::io::Read>(
        reader: R,
    ) -> Result<chunk::Chunker, serde_json::Error> {
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
    pub(in crate::compile) fn new(matcher: PosMatcher) -> Self {
        POSFilter { matcher }
    }
}

impl Regex {
    pub(in crate::compile) fn from_java_regex(
        java_regex_str: &str,
        full_match: bool,
        case_sensitive: bool,
    ) -> Result<Self, Error> {
        let regex_string =
            super::utils::from_java_regex(java_regex_str, case_sensitive, full_match)?;

        let regex = Regex::new(regex_string);
        if let Err(error) = regex.try_compile() {
            return Err(Error::Regex(error));
        }

        Ok(regex)
    }
}

impl Engine {
    pub(in crate::compile) fn to_graph_id(&self, id: usize) -> Result<GraphId, Error> {
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

        pub(in crate::compile) fn mut_graph_ids(&mut self) -> Vec<&mut GraphId> {
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
        pub(in crate::compile) fn new_regex(
            regex: Regex,
            negate: bool,
            empty_always_false: bool,
        ) -> Self {
            Matcher {
                matcher: either::Right(regex),
                negate,
                case_sensitive: true, // handled by regex, should maybe be an option
                empty_always_false,
            }
        }

        pub(in crate::compile) fn new_string(
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

        pub(in crate::compile) fn graph_id(&self) -> Option<GraphId> {
            if let either::Left(either::Right(id)) = &self.matcher {
                Some(*id)
            } else {
                None
            }
        }

        pub(in crate::compile) fn mut_graph_id(&mut self) -> Option<&mut GraphId> {
            if let either::Left(either::Right(id)) = &mut self.matcher {
                Some(id)
            } else {
                None
            }
        }
    }

    impl Quantifier {
        pub(in crate::compile) fn new(min: usize, max: usize) -> Self {
            assert!(max >= min);
            Quantifier { min, max }
        }
    }

    impl AndAtom {
        pub(in crate::compile) fn and(atoms: Vec<Atom>) -> Atom {
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
        pub(in crate::compile) fn or(atoms: Vec<Atom>) -> Atom {
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
        pub(in crate::compile) fn not(atom: Atom) -> Atom {
            match atom {
                Atom::TrueAtom { .. } => FalseAtom::default().into(),
                Atom::FalseAtom { .. } => TrueAtom::default().into(),
                x => (NotAtom { atom: Box::new(x) }).into(),
            }
        }
    }

    impl OffsetAtom {
        pub(in crate::compile) fn new(atom: Atom, offset: isize) -> Self {
            OffsetAtom {
                atom: Box::new(atom),
                offset,
            }
        }
    }

    impl Composition {
        pub(in crate::compile) fn new(mut parts: Vec<Part>) -> Result<Self, Error> {
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

pub(in crate::compile) mod filters {
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

    pub(in crate::compile) fn get_filter(
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
