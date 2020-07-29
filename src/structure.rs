use serde::Deserialize;
use std::fs::File;
use std::io::BufReader;
use xml::reader::EventReader;

mod preprocess {
    use xml::reader::EventReader;
    use xml::writer::EmitterConfig;

    #[derive(Debug, Clone)]
    pub struct ExtraInfo {
        pub id: Option<String>,
    }

    pub fn sanitize(input: impl std::io::Read) -> String {
        let mut sanitized = Vec::new();

        let mut writer = EmitterConfig::new()
            .perform_indent(true)
            .create_writer(&mut sanitized);

        let parser = EventReader::new(input);

        let events = parser
            .into_iter()
            .map(|x| x.expect("error reading XML"))
            .filter(|x| {
                // processing instructions break the writer and are useless to us
                if let xml::reader::XmlEvent::ProcessingInstruction { .. } = x {
                    false
                } else {
                    true
                }
            })
            .collect::<Vec<_>>();

        let mut out_events: Vec<xml::writer::XmlEvent> = Vec::new();

        for event in &events {
            if let xml::reader::XmlEvent::Characters(chars) = &event {
                out_events.push(xml::writer::XmlEvent::start_element("text").into());
                out_events.push(xml::writer::XmlEvent::characters(chars));
                out_events.push(xml::writer::XmlEvent::end_element().into());
            } else if let Some(writer_event) = event.as_writer_event() {
                out_events.push(writer_event);
            }
        }

        for event in out_events {
            writer.write(event).expect("error writing to output XML");
        }

        std::str::from_utf8(&sanitized)
            .expect("invalid UTF-8")
            .to_string()
    }

    pub fn extract_rules(xml: impl std::io::Read) -> Vec<(String, ExtraInfo)> {
        let mut rules = Vec::new();

        let parser = EventReader::new(xml);

        let mut current = Vec::new();
        let mut extra_info = None;

        for event in parser {
            let event = event.expect("error reading XML");
            current.push(event.clone());

            match event {
                xml::reader::XmlEvent::StartElement {
                    name, attributes, ..
                } => {
                    if name.to_string() == "rule" {
                        while current.len() > 1 {
                            current.remove(0);
                        }

                        let id = attributes
                            .iter()
                            .find(|x| x.name.local_name == "id")
                            .map(|x| x.value.clone());

                        extra_info = Some(ExtraInfo { id })
                    }
                }
                xml::reader::XmlEvent::EndElement { name, .. } => {
                    if name.to_string() == "rule" {
                        let mut buffer: Vec<u8> = Vec::new();

                        let mut writer = EmitterConfig::new()
                            .perform_indent(true)
                            .create_writer(&mut buffer);

                        for event in &current {
                            if let Some(writer_event) = event.as_writer_event() {
                                writer.write(writer_event).expect("error writing XML");
                            }
                        }

                        rules.push((
                            std::str::from_utf8(&buffer)
                                .expect("invalid UTF-8")
                                .to_string(),
                            extra_info.clone().expect("must have ExtraInfo."),
                        ));
                    }
                }
                _ => {}
            }
        }

        rules
    }
}

#[derive(Debug, Deserialize)]
pub struct XMLString {
    text: String,
}

impl std::ops::Deref for XMLString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

impl std::convert::Into<String> for XMLString {
    fn into(self) -> String {
        self.text
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Suggestion {
    pub text: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum MessagePart {
    Suggestion(Suggestion),
    Text(String),
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Message {
    #[serde(rename = "$value")]
    pub parts: Vec<MessagePart>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ExampleMarker {
    pub text: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum ExamplePart {
    Marker(ExampleMarker),
    Text(String),
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Example {
    pub correction: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Vec<ExamplePart>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Token {
    pub text: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PatternMarker {
    #[serde(rename = "token")]
    pub tokens: Vec<Token>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum PatternPart {
    Token(Token),
    Marker(PatternMarker),
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Pattern {
    // case_sensitive: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Vec<PatternPart>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Rule {
    pub pattern: Pattern,
    pub message: Message,
    #[serde(rename = "example")]
    pub examples: Vec<Example>,
    pub id: Option<String>,
    pub name: String,
    pub short: Option<XMLString>,
}

pub fn read_rules<P: AsRef<std::path::Path>>(path: P) -> Vec<Result<Rule, serde_xml_rs::Error>> {
    let ids: Option<&[&str]> = None;
    let file = File::open(path).unwrap();
    let file = BufReader::new(file);

    let sanitized = preprocess::sanitize(file);
    let rules = preprocess::extract_rules(sanitized.as_bytes());

    rules
        .iter()
        .filter(|x| {
            if let Some(ids) = ids {
                if let Some(current_id) = &x.1.id {
                    ids.contains(&current_id.as_str())
                } else {
                    false
                }
            } else {
                true
            }
        })
        .map(|x| {
            Rule::deserialize(&mut serde_xml_rs::Deserializer::new(EventReader::new(
                x.0.as_bytes(),
            )))
        })
        .collect()
}
