use serde::Deserialize;
use std::fs::File;
use std::io::BufReader;
use xml::reader::EventReader;

#[derive(Debug, Clone)]
pub struct Id {
    name: String,
    num: usize,
}

impl Id {
    fn new(name: String, num: usize) -> Self {
        Id { name, num }
    }
}

impl std::string::ToString for Id {
    fn to_string(&self) -> String {
        format!("{}.{}", self.name, self.num)
    }
}

mod preprocess {
    use super::Id;
    use xml::reader::EventReader;
    use xml::writer::EmitterConfig;

    pub fn sanitize(input: impl std::io::Read, whitespace_sensitive_tags: &[&str]) -> String {
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
        let mut parents = Vec::new();

        for event in &events {
            match event {
                xml::reader::XmlEvent::StartElement { name, .. } => {
                    parents.push(name.local_name.as_str());
                }
                xml::reader::XmlEvent::EndElement { .. } => {
                    parents.pop();
                }
                xml::reader::XmlEvent::Characters(chars) => {
                    out_events.push(
                        xml::writer::XmlEvent::start_element("text")
                            .attr("text", chars)
                            .into(),
                    );
                    out_events.push(xml::writer::XmlEvent::end_element().into());
                    continue;
                }
                xml::reader::XmlEvent::Whitespace(whitespace) => {
                    if parents
                        .iter()
                        .any(|x| whitespace_sensitive_tags.contains(x))
                    {
                        out_events.push(
                            xml::writer::XmlEvent::start_element("text")
                                .attr("text", whitespace)
                                .into(),
                        );
                        out_events.push(xml::writer::XmlEvent::end_element().into());
                        continue;
                    }
                }
                _ => {}
            }

            if let Some(writer_event) = event.as_writer_event() {
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

    pub fn extract_rules(mut xml: impl std::io::Read) -> Vec<(String, Id)> {
        let mut string = String::new();
        xml.read_to_string(&mut string)
            .expect("error writing to string.");

        let document = roxmltree::Document::parse(&string).expect("error parsing XML");

        document
            .descendants()
            .filter(|x| {
                let name = x.tag_name().name();

                name == "rulegroup"
                    || (name == "rule"
                        && x.parent_element()
                            .expect("must have parent")
                            .tag_name()
                            .name()
                            != "rulegroup")
            })
            .map(|x| {
                let xml = string[x.range()].to_string();
                let id = Id::new(
                    x.parent_element()
                        .expect("must have parent")
                        .attribute("id")
                        .unwrap_or("")
                        .to_string(),
                    0,
                );

                (xml, id)
            })
            .collect()
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct XMLString {
    pub text: String,
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

#[derive(Debug, Clone, Deserialize)]
pub struct XMLText {
    pub text: XMLString,
}

impl std::ops::Deref for XMLText {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

impl std::convert::Into<String> for XMLText {
    fn into(self) -> String {
        self.text.into()
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Match {
    pub no: String,
    pub case_conversion: Option<String>,
    pub regexp_match: Option<String>,
    pub regexp_replace: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum SuggestionPart {
    Match(Match),
    Text(XMLString),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Suggestion {
    #[serde(rename = "$value")]
    pub parts: Vec<SuggestionPart>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum MessagePart {
    Suggestion(Suggestion),
    Text(XMLString),
    Match(Match),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Message {
    #[serde(rename = "$value")]
    pub parts: Vec<MessagePart>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ExampleMarker {
    pub text: XMLString,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "lowercase")]
pub enum ExamplePart {
    Marker(ExampleMarker),
    Text(XMLString),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Example {
    pub correction: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Vec<ExamplePart>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Exception {
    pub case_sensitive: Option<String>,
    pub inflected: Option<String>,
    pub postag: Option<String>,
    pub chunk: Option<String>,
    pub regexp: Option<String>,
    pub spacebefore: Option<String>,
    pub negate: Option<String>,
    pub scope: Option<String>,
    pub text: Option<XMLString>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum TokenPart {
    Text(XMLString),
    Exception(Exception),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Token {
    pub min: Option<String>,
    pub max: Option<String>,
    pub skip: Option<String>,
    pub case_sensitive: Option<String>,
    pub inflected: Option<String>,
    pub postag: Option<String>,
    pub chunk: Option<String>,
    pub regexp: Option<String>,
    pub spacebefore: Option<String>,
    pub negate: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Option<Vec<TokenPart>>,
}

// NB: needlessly verbose, would be nicer with #[serde(flatten)] but blocked by https://github.com/RReverser/serde-xml-rs/issues/83
pub trait MatchAttributes {
    fn case_sensitive(&self) -> &Option<String>;
    fn inflected(&self) -> &Option<String>;
    fn postag(&self) -> &Option<String>;
    fn chunk(&self) -> &Option<String>;
    fn regexp(&self) -> &Option<String>;
    fn spacebefore(&self) -> &Option<String>;
    fn negate(&self) -> &Option<String>;
}

macro_rules! impl_match_attributes {
    ($e:ty) => {
        impl MatchAttributes for $e {
            fn case_sensitive(&self) -> &Option<String> {
                &self.case_sensitive
            }

            fn inflected(&self) -> &Option<String> {
                &self.inflected
            }

            fn postag(&self) -> &Option<String> {
                &self.postag
            }

            fn chunk(&self) -> &Option<String> {
                &self.chunk
            }

            fn regexp(&self) -> &Option<String> {
                &self.regexp
            }

            fn spacebefore(&self) -> &Option<String> {
                &self.spacebefore
            }

            fn negate(&self) -> &Option<String> {
                &self.negate
            }
        }
    };
}

impl_match_attributes!(&Exception);
impl_match_attributes!(&Token);

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PatternMarker {
    #[serde(rename = "token")]
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum PatternPart {
    Token(Token),
    Marker(PatternMarker),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Pattern {
    pub case_sensitive: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Vec<PatternPart>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Rule {
    pub pattern: Pattern,
    #[serde(rename = "antipattern")]
    pub antipatterns: Option<Vec<Pattern>>,
    pub message: Message,
    #[serde(rename = "suggestion")]
    pub suggestions: Option<Vec<Suggestion>>,
    #[serde(rename = "example")]
    pub examples: Vec<Example>,
    pub id: Option<String>,
    pub name: Option<String>,
    pub short: Option<XMLText>,
    pub url: Option<XMLText>,
    pub default: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RuleGroup {
    pub id: String,
    #[serde(rename = "antipattern")]
    pub antipatterns: Option<Vec<Pattern>>,
    pub default: Option<String>,
    pub name: Option<String>,
    pub short: Option<XMLText>,
    pub url: Option<XMLText>,
    #[serde(rename = "rule")]
    pub rules: Vec<Rule>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum RuleContainer {
    Rule(Rule),
    RuleGroup(RuleGroup),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DisambiguationExample {
    #[serde(rename = "type")]
    pub kind: String,
    pub inputform: Option<String>,
    pub outputform: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Vec<ExamplePart>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Disambiguation {
    pub postag: String,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DisambiguationRule {
    pub pattern: Pattern,
    #[serde(rename = "antipattern")]
    pub antipatterns: Option<Vec<Pattern>>,
    #[serde(rename = "suggestion")]
    pub suggestions: Option<Vec<Suggestion>>,
    pub disambig: Disambiguation,
    #[serde(rename = "example")]
    pub examples: Option<Vec<DisambiguationExample>>,
    pub id: Option<String>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DisambiguationRuleGroup {
    pub id: String,
    #[serde(rename = "antipattern")]
    pub antipatterns: Option<Vec<Pattern>>,
    pub name: Option<String>,
    #[serde(rename = "rule")]
    pub rules: Vec<DisambiguationRule>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum DisambiguationRuleContainer {
    Rule(DisambiguationRule),
    RuleGroup(DisambiguationRuleGroup),
}

macro_rules! flatten_group {
    ($group:expr) => {{
        let rule_group_id = $group.id.clone();
        let group_antipatterns = if let Some(antipatterns) = $group.antipatterns {
            antipatterns
        } else {
            Vec::new()
        };

        $group
            .rules
            .into_iter()
            .enumerate()
            .map(|(i, mut rule)| {
                let id = Id::new(rule_group_id.clone(), i);
                if let Some(antipatterns) = &mut rule.antipatterns {
                    antipatterns.extend(group_antipatterns.clone());
                } else {
                    rule.antipatterns = Some(group_antipatterns.clone());
                }

                (rule, id.to_string())
            })
            .collect::<Vec<_>>()
    }};
}

pub fn read_rules<P: AsRef<std::path::Path>>(
    path: P,
) -> Vec<Result<(Rule, String), serde_xml_rs::Error>> {
    let file = File::open(path).unwrap();
    let file = BufReader::new(file);

    let sanitized = preprocess::sanitize(file, &["suggestion"]);
    let rules = preprocess::extract_rules(sanitized.as_bytes());

    rules
        .into_iter()
        .map(|x| {
            let mut out = Vec::new();

            let deseralized = RuleContainer::deserialize(&mut serde_xml_rs::Deserializer::new(
                EventReader::new(x.0.as_bytes()),
            ));

            out.extend(match deseralized {
                Ok(rule_container) => match rule_container {
                    RuleContainer::Rule(rule) => {
                        let id = rule.id.clone().unwrap_or_else(String::new);
                        vec![Ok((rule, id))]
                    }
                    RuleContainer::RuleGroup(rule_group) => {
                        flatten_group!(rule_group).into_iter().map(Ok).collect()
                    }
                },
                Err(err) => vec![Err(err)],
            });
            out
        })
        .flatten()
        .collect()
}

pub fn read_disambiguation_rules<P: AsRef<std::path::Path>>(
    path: P,
) -> Vec<Result<(DisambiguationRule, String), serde_xml_rs::Error>> {
    let file = File::open(path).unwrap();
    let file = BufReader::new(file);

    let sanitized = preprocess::sanitize(file, &[]);
    let rules = preprocess::extract_rules(sanitized.as_bytes());

    rules
        .into_iter()
        .map(|x| {
            let mut out = Vec::new();

            let deseralized = DisambiguationRuleContainer::deserialize(
                &mut serde_xml_rs::Deserializer::new(EventReader::new(x.0.as_bytes())),
            );

            out.extend(match deseralized {
                Ok(rule_container) => match rule_container {
                    DisambiguationRuleContainer::Rule(rule) => {
                        let id = rule.id.clone().unwrap_or_else(String::new);
                        vec![Ok((rule, id))]
                    }
                    DisambiguationRuleContainer::RuleGroup(rule_group) => {
                        flatten_group!(rule_group).into_iter().map(Ok).collect()
                    }
                },
                Err(err) => vec![Err(err)],
            });
            out
        })
        .flatten()
        .collect()
}
