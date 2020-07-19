use serde::Deserialize;
use serde_xml_rs::from_reader;
use std::fs::File;
use std::io::BufReader;
use xml::reader::EventReader;
use xml::writer::EmitterConfig;

#[derive(Debug, Deserialize)]
pub struct Suggestion {}

#[derive(Debug, Deserialize)]
enum MessagePart {
    Suggestion(Suggestion),
    Text(String),
}

#[derive(Debug, Deserialize)]
pub struct Message {
    #[serde(rename = "$value")]
    suggestion: Vec<MessagePart>,
}

#[derive(Debug, Deserialize)]
pub struct Rule {}

#[derive(Deserialize)]
struct RuleVec {
    #[serde(rename = "$value")]
    rules: Vec<serde_value::Value>,
}

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum CategoryChild {
    Rule(serde_value::Value),
    RuleGroup(RuleVec),
}

#[derive(Deserialize)]
struct Category {
    #[serde(rename = "$value")]
    rules: Vec<CategoryChild>,
}

#[derive(Deserialize)]
pub struct Rules {
    #[serde(rename = "category")]
    categories: Vec<Category>,
}

impl Rules {
    pub fn get_rules(&self) -> Vec<Rule> {
        let mut parsed_rules = Vec::new();

        for category in &self.categories {
            for category_child in &category.rules {
                let rules_to_add = match category_child {
                    CategoryChild::Rule(value) => vec![Rule::deserialize(value.clone()).unwrap()],
                    CategoryChild::RuleGroup(values) => values
                        .rules
                        .iter()
                        .map(|value| Rule::deserialize(value.clone()).unwrap())
                        .collect(),
                };

                parsed_rules.extend(rules_to_add);
            }
        }

        parsed_rules
    }
}

fn sanitize(input: impl std::io::Read) -> Vec<u8> {
    let mut sanitized = Vec::new();

    let mut writer = EmitterConfig::new()
        .perform_indent(true)
        .create_writer(&mut sanitized);

    let parser = EventReader::new_with_config(
        input,
        xml::ParserConfig {
            ignore_comments: false,
            whitespace_to_characters: false,
            ..Default::default()
        },
    );

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

    sanitized
}

fn main() {
    let file = File::open("data/grammar.canonic.xml").unwrap();
    let file = BufReader::new(file);

    let sanitized = sanitize(file);

    let rules: Rules = from_reader(&sanitized[..]).unwrap();
    println!("{:#?}", rules.get_rules());
}
