use crate::types::*;
use aho_corasick::AhoCorasick;
use serde::{Deserialize, Serialize};

use super::tag::Tagger;

#[derive(Serialize, Deserialize)]
pub(crate) struct MultiwordTaggerFields {
    pub(crate) multiwords: Vec<(String, owned::PosId)>,
}

impl From<MultiwordTaggerFields> for MultiwordTagger {
    fn from(data: MultiwordTaggerFields) -> Self {
        MultiwordTagger {
            matcher: AhoCorasick::new_auto_configured(
                &data
                    .multiwords
                    .iter()
                    .map(|(word, _)| word)
                    .collect::<Vec<_>>(),
            ),
            multiwords: data.multiwords,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(from = "MultiwordTaggerFields")]
pub struct MultiwordTagger {
    #[serde(skip)]
    matcher: AhoCorasick,
    multiwords: Vec<(String, owned::PosId)>,
}

impl MultiwordTagger {
    pub fn apply<'t>(&'t self, tokens: &mut Vec<IncompleteToken<'t>>, tagger: &'t Tagger) {
        let mut start_indices = DefaultHashMap::new();
        let mut end_indices = DefaultHashMap::new();
        let mut byte_index = 0;

        let joined = tokens
            .iter()
            .enumerate()
            .map(|(i, x)| {
                start_indices.insert(byte_index, i);
                byte_index += x.word.text.0.len();
                end_indices.insert(byte_index, i);
                byte_index += " ".len();

                x.word.text.0.as_ref()
            })
            .collect::<Vec<_>>()
            .join(" ");

        for m in self.matcher.find_iter(&joined) {
            if let (Some(start), Some(end)) =
                (start_indices.get(&m.start()), end_indices.get(&m.end()))
            {
                let (word, pos) = &self.multiwords[m.pattern()];
                // end index is inclusive
                for token in tokens[*start..(*end + 1)].iter_mut() {
                    token.multiword_data = Some(WordData::new(
                        tagger.id_word(word.as_str().into()),
                        pos.as_ref_id(),
                    ));
                }
            }
        }
    }
}
