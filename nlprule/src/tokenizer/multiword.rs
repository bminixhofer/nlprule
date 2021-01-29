use crate::types::*;
use serde::{Deserialize, Serialize};

use super::tag::Tagger;

#[derive(Serialize, Deserialize)]
pub struct MultiwordTagger {
    pub(crate) multiwords: Vec<(Vec<owned::WordId>, owned::PosId)>,
}

impl MultiwordTagger {
    fn is_match(&self, tokens: &[IncompleteToken], multiword: &[owned::WordId]) -> bool {
        if multiword.is_empty() {
            return true;
        }

        if tokens.is_empty() {
            return false;
        }

        tokens[0].word.text == multiword[0].as_ref_id()
            && self.is_match(&tokens[1..], &multiword[1..])
    }

    pub fn apply<'t>(&'t self, tokens: &mut Vec<IncompleteToken<'t>>, tagger: &'t Tagger) {
        for i in 0..tokens.len() {
            for (multiword, pos) in &self.multiwords {
                if self.is_match(&tokens[i..], &multiword) {
                    for token in &mut tokens[i..i + multiword.len()] {
                        let joined_word: String = multiword
                            .iter()
                            .map(|x| x.0.as_str())
                            .collect::<Vec<_>>()
                            .join(" ");
                        token.multiword_data = Some(WordData::new(
                            tagger.id_word(joined_word.into()),
                            pos.as_ref_id(),
                        ));
                    }
                }
            }
        }
    }
}
