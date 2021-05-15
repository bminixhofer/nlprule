use std::{io::BufReader, path::PathBuf};

use fs_err::File;
use serde::Deserialize;

use crate::compile::{BuildComponent, BuildInfo, Error};

use super::*;

#[derive(Serialize, Deserialize)]
struct ContextData {
    parameters: Vec<f32>,
    outcomes: Vec<usize>,
}

#[derive(Deserialize)]
struct ModelData {
    outcome_labels: Vec<String>,
    pmap: DefaultHashMap<String, ContextData>,
}

impl From<ModelData> for Model {
    fn from(data: ModelData) -> Self {
        let mut outcomes: Vec<usize> = Vec::new();
        let mut parameters: Vec<f32> = Vec::new();

        let pmap = data
            .pmap
            .into_iter()
            .map(|(key, value)| {
                assert_eq!(value.outcomes.len(), value.parameters.len());

                let offset = outcomes.len();
                let length = value.outcomes.len();

                outcomes.extend(value.outcomes);
                parameters.extend(value.parameters);

                (hash::hash_str(&key), (offset, length))
            })
            .collect::<DefaultHashMap<_, _>>();

        Model {
            outcome_labels: data.outcome_labels,
            outcomes,
            parameters,
            pmap,
        }
    }
}

#[derive(Deserialize)]
pub struct Paths {
    chunker: PathBuf,
}

impl BuildComponent for Chunker {
    type Paths = Paths;

    fn build(paths: Paths, _build_info: Option<&mut BuildInfo>) -> Result<Chunker, Error> {
        #[derive(Deserialize)]
        struct ChunkData {
            token_model: ModelData,
            pos_model: ModelData,
            pos_tagdict: DefaultHashMap<String, Vec<String>>,
            chunk_model: ModelData,
        }

        let chunk_data: ChunkData =
            serde_json::from_reader(BufReader::new(File::open(paths.chunker)?))?;
        Ok(Chunker {
            token_model: MaxentTokenizer {
                model: chunk_data.token_model.into(),
            },
            pos_model: MaxentPosTagger {
                model: chunk_data.pos_model.into(),
                tagdict: chunk_data.pos_tagdict,
            },
            chunk_model: MaxentChunker {
                model: chunk_data.chunk_model.into(),
            },
        })
    }
}
