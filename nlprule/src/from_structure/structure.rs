use serde::Deserialize;
use std::fs::File;
use std::io::BufReader;
use xml::reader::EventReader;

mod preprocess {
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
                !matches!(x, xml::reader::XmlEvent::ProcessingInstruction { .. })
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

    pub fn extract_rules(mut xml: impl std::io::Read) -> Vec<(String, bool)> {
        let mut string = String::new();
        xml.read_to_string(&mut string)
            .expect("error writing to string.");

        let document = roxmltree::Document::parse(&string).expect("error parsing XML");

        document
            .descendants()
            .filter(|x| {
                let name = x.tag_name().name();

                name == "unification"
                    || name == "rulegroup"
                    || (name == "rule"
                        && x.parent_element()
                            .expect("must have parent")
                            .tag_name()
                            .name()
                            != "rulegroup")
            })
            .map(|x| {
                let xml = string[x.range()].to_string();
                let parent = x.parent_element().expect("must have parent");

                (
                    xml,
                    parent.attribute("default").map_or(true, |x| x != "off"),
                )
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
    pub postag: Option<String>,
    #[serde(rename = "postag_regexp")]
    pub postag_regex: Option<String>,
    pub postag_replace: Option<String>,
    pub text: Option<XMLString>,
    pub include_skipped: Option<String>,
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
    #[serde(rename = "type")]
    pub kind: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Exception {
    pub case_sensitive: Option<String>,
    pub inflected: Option<String>,
    pub postag: Option<String>,
    pub postag_regexp: Option<String>,
    pub chunk: Option<String>,
    pub regexp: Option<String>,
    pub spacebefore: Option<String>,
    pub negate: Option<String>,
    pub negate_pos: Option<String>,
    pub scope: Option<String>,
    pub text: Option<XMLString>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
#[allow(clippy::large_enum_variant)]
pub enum TokenPart {
    Text(XMLString),
    Exception(Exception),
    #[serde(rename = "match")]
    Sub(Sub),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Sub {
    pub no: String,
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
    pub postag_regexp: Option<String>,
    pub chunk: Option<String>,
    pub regexp: Option<String>,
    pub spacebefore: Option<String>,
    pub negate: Option<String>,
    pub negate_pos: Option<String>,
    #[serde(rename = "$value")]
    pub parts: Option<Vec<TokenPart>>,
}

// NB: needlessly verbose, would be nicer with #[serde(flatten)] but blocked by https://github.com/RReverser/serde-xml-rs/issues/83
pub trait MatchAttributes {
    fn case_sensitive(&self) -> &Option<String>;
    fn inflected(&self) -> &Option<String>;
    fn postag(&self) -> &Option<String>;
    fn postag_regexp(&self) -> &Option<String>;
    fn chunk(&self) -> &Option<String>;
    fn regexp(&self) -> &Option<String>;
    fn spacebefore(&self) -> &Option<String>;
    fn negate(&self) -> &Option<String>;
    fn negate_pos(&self) -> &Option<String>;
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

            fn postag_regexp(&self) -> &Option<String> {
                &self.postag_regexp
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

            fn negate_pos(&self) -> &Option<String> {
                &self.negate_pos
            }
        }
    };
}

impl_match_attributes!(&Exception);
impl_match_attributes!(&Token);

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TokenVector {
    #[serde(rename = "token")]
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Feature {
    pub id: String,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Ignore {
    #[serde(rename = "$value")]
    pub tokens: Vec<TokenCombination>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum UnifyTokenCombination {
    Token(Token),
    Or(TokenVector),
    And(TokenVector),
    Feature(Feature),
    #[serde(rename = "unify-ignore")]
    Ignore(Ignore),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Unify {
    #[serde(rename = "$value")]
    pub tokens: Vec<UnifyTokenCombination>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum TokenCombination {
    Token(Token),
    Or(TokenVector),
    And(TokenVector),
    Unify(Unify),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PatternMarker {
    #[serde(rename = "$value")]
    pub tokens: Vec<TokenCombination>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum PatternPart {
    Token(Token),
    Marker(PatternMarker),
    Or(TokenVector),
    And(TokenVector),
    Unify(Unify),
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
    pub pattern: Option<Pattern>,
    #[serde(rename = "regexp")]
    pub regex: Option<XMLText>,
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
    pub filter: Option<Filter>,
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
pub struct WordData {
    pub pos: String,
    pub text: Option<String>,
    pub lemma: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Filter {
    pub args: String,
    pub class: String,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DisambiguationMatch {
    pub no: usize,
    pub postag: Option<String>,
    pub postag_regexp: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub enum DisambiguationPart {
    #[serde(rename = "wd")]
    WordData(WordData),
    #[serde(rename = "match")]
    Match(DisambiguationMatch),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Disambiguation {
    pub postag: Option<String>,
    pub action: Option<String>,
    #[serde(rename = "$value")]
    pub word_datas: Option<Vec<DisambiguationPart>>,
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
    pub filter: Option<Filter>,
    #[serde(rename = "__unused_unifications")]
    pub unifications: Option<Vec<Unification>>,
    pub default: Option<String>,
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
    pub default: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EquivalenceToken {
    pub postag: String,
    pub postag_regexp: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Equivalence {
    pub token: EquivalenceToken,
    #[serde(rename = "type")]
    pub kind: String,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Unification {
    #[serde(rename = "equivalence")]
    pub equivalences: Vec<Equivalence>,
    pub feature: String,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(deny_unknown_fields)]
pub enum DisambiguationRuleContainer {
    Rule(DisambiguationRule),
    RuleGroup(DisambiguationRuleGroup),
    Unification(Unification),
}

macro_rules! flatten_group {
    ($group:expr, $category_on:expr) => {{
        let rule_group_id = $group.id.clone();
        let group_antipatterns = if let Some(antipatterns) = $group.antipatterns {
            antipatterns
        } else {
            Vec::new()
        };

        let group_on = $group.default.as_ref().map_or(true, |x| x != "off");

        $group
            .rules
            .into_iter()
            .enumerate()
            .map(|(i, mut rule)| {
                if let Some(antipatterns) = &mut rule.antipatterns {
                    antipatterns.extend(group_antipatterns.clone());
                } else {
                    rule.antipatterns = Some(group_antipatterns.clone());
                }
                let on =
                    group_on && $category_on && rule.default.as_ref().map_or(true, |x| x != "off");

                (rule, format!("{}.{}", rule_group_id, i), on)
            })
            .collect::<Vec<_>>()
    }};
}

pub fn read_rules<P: AsRef<std::path::Path>>(
    path: P,
) -> Vec<Result<(Rule, String, bool), serde_xml_rs::Error>> {
    let file = File::open(path).unwrap();
    let file = BufReader::new(file);

    let sanitized = preprocess::sanitize(file, &["suggestion"]);
    let rules = preprocess::extract_rules(sanitized.as_bytes());

    rules
        .into_iter()
        .map(|(xml, category_on)| {
            let mut out = Vec::new();

            let deseralized = RuleContainer::deserialize(&mut serde_xml_rs::Deserializer::new(
                EventReader::new(xml.as_bytes()),
            ));

            out.extend(match deseralized {
                Ok(rule_container) => match rule_container {
                    RuleContainer::Rule(rule) => {
                        let id = rule.id.clone().unwrap_or_else(String::new);
                        let on = category_on && rule.default.as_ref().map_or(true, |x| x != "off");
                        vec![Ok((rule, id, on))]
                    }
                    RuleContainer::RuleGroup(rule_group) => flatten_group!(rule_group, category_on)
                        .into_iter()
                        .map(Ok)
                        .collect(),
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

    let mut unifications = Vec::new();

    let rules: Vec<_> = rules
        .into_iter()
        .map(|(xml, _)| {
            let mut out = Vec::new();

            let deseralized = DisambiguationRuleContainer::deserialize(
                &mut serde_xml_rs::Deserializer::new(EventReader::new(xml.as_bytes())),
            );

            out.extend(match deseralized {
                Ok(rule_container) => match rule_container {
                    DisambiguationRuleContainer::Rule(rule) => {
                        let id = rule.id.clone().unwrap_or_else(String::new);
                        vec![Ok((rule, id))]
                    }
                    DisambiguationRuleContainer::RuleGroup(rule_group) => {
                        flatten_group!(rule_group, true)
                            .into_iter()
                            .map(|x| (Ok((x.0, x.1))))
                            .collect()
                    }
                    DisambiguationRuleContainer::Unification(unification) => {
                        unifications.push(unification);

                        vec![]
                    }
                },
                Err(err) => vec![Err(err)],
            });
            out
        })
        .flatten()
        .collect();

    rules
        .into_iter()
        .map(|result| match result {
            Ok(mut x) => {
                x.0.unifications = Some(unifications.clone());

                Ok(x)
            }
            Err(x) => Err(x),
        })
        .collect()
}
