use serde::Deserialize;
use std::fs::File;
use std::io::BufReader;
use xml::reader::EventReader;

mod preprocess {
    use xml::reader::EventReader;
    use xml::writer::EmitterConfig;

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
            .enumerate()
            .collect::<Vec<_>>();

        let mut out_events: Vec<xml::writer::XmlEvent> = Vec::new();

        for (i, event) in &events {
            if let xml::reader::XmlEvent::Characters(chars) = &event {
                if let xml::reader::XmlEvent::StartElement { name, .. } = &events[i - 1].1 {
                    let start_name = name;

                    if let xml::reader::XmlEvent::EndElement { name, .. } = &events[i + 1].1 {
                        let end_name = name;

                        if start_name == end_name {
                            out_events.push(xml::writer::XmlEvent::characters(chars));
                            continue;
                        }
                    }
                }

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

    pub fn extract_rules(xml: impl std::io::Read) -> Vec<String> {
        let mut rules = Vec::new();

        let parser = EventReader::new(xml);

        let mut current = Vec::new();

        for event in parser {
            let event = event.expect("error reading XML");
            current.push(event.clone());

            match event {
                xml::reader::XmlEvent::StartElement { name, .. } => {
                    if name.to_string() == "rule" {
                        while current.len() > 1 {
                            current.remove(0);
                        }
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

                        rules.push(
                            std::str::from_utf8(&buffer)
                                .expect("invalid UTF-8")
                                .to_string(),
                        );
                    }
                }
                _ => {}
            }
        }

        rules
    }
}

#[derive(Debug, Deserialize)]
pub struct Suggestion {}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum MessagePart {
    Suggestion(Suggestion),
    Text(String),
}

#[derive(Debug, Deserialize)]
pub struct Message {
    #[serde(rename = "$value")]
    parts: Vec<MessagePart>,
}

#[derive(Debug, Deserialize)]
pub struct ExampleMarker {
    #[serde(rename = "$value")]
    text: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum ExamplePart {
    Marker(ExampleMarker),
    Text(String),
}

#[derive(Debug, Deserialize)]
pub struct Example {
    correction: String,
    #[serde(rename = "$value")]
    parts: Vec<ExamplePart>,
}

#[derive(Debug, Deserialize)]
pub struct Token {
    #[serde(rename = "$value")]
    text: String,
}

#[derive(Debug, Deserialize)]
pub struct PatternMarker {
    #[serde(rename = "token")]
    tokens: Vec<Token>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PatternPart {
    Token(Token),
    Marker(PatternMarker),
}

#[derive(Debug, Deserialize)]
pub struct Pattern {
    #[serde(rename = "$value")]
    parts: Vec<PatternPart>,
}

#[derive(Debug, Deserialize)]
pub struct Rule {
    pattern: Pattern,
    message: Message,
    #[serde(rename = "example")]
    examples: Vec<Example>,
    id: Option<String>,
    short: String,
}

fn main() {
    let file = File::open("data/grammar.canonic.xml").unwrap();
    let file = BufReader::new(file);

    let sanitized = preprocess::sanitize(file);
    let rules = preprocess::extract_rules(sanitized.as_bytes());
    let rules = rules
        .iter()
        .map(|x| {
            Rule::deserialize(&mut serde_xml_rs::Deserializer::new(EventReader::new(
                x.as_bytes(),
            )))
        })
        .collect::<Vec<_>>();

    println!("{:#?}", rules);
}
