//! Provides structures to identify nodes at various levels of the rule tree and a [Selector] to match on these structs.
//!
//! Rules are nested in up to three layers in the original XML:
//! ```xml
//! <category ...>
//!    <rulegroup ...> <!-- can be omitted, there's also <rule>s at this level -->
//!       <rule ...>
//!       </rule>
//!    </rulegroup>
//! </category>
//! ```
//!
//! The [Category], [Group] and [Index] structs provide a way to identify these layers.

use serde::{Deserialize, Serialize};
use std::{
    convert::TryFrom,
    fmt,
    hash::{Hash, Hasher},
    num::ParseIntError,
};
use unicase::UniCase;

#[derive(Debug, Clone, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
    #[error("error parsing selector from string: {0}")]
    ParseStringError(String),
}

/// Identifies a category.
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialOrd, Ord)]
pub struct Category(String);

impl Eq for Category {}
impl PartialEq<Category> for Category {
    fn eq(&self, other: &Category) -> bool {
        UniCase::new(&self.0) == UniCase::new(&other.0)
    }
}

impl Hash for Category {
    fn hash<H: Hasher>(&self, state: &mut H) {
        UniCase::new(&self.0).hash(state)
    }
}

/// Identifies a rule group.
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialOrd, Ord)]
pub struct Group {
    parent: Category,
    inner: String,
}

impl Eq for Group {}
impl PartialEq<Group> for Group {
    fn eq(&self, other: &Group) -> bool {
        self.parent.eq(&other.parent) && UniCase::new(&self.inner) == UniCase::new(&other.inner)
    }
}

impl Hash for Group {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        UniCase::new(&self.inner).hash(state);
    }
}

/// Identifies the children of a rule group.
/// Rules without a group in the original XML are treated as a rule group with one child.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct Index {
    parent: Group,
    inner: usize,
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Group {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.parent, self.inner)
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.parent, self.inner)
    }
}

impl Category {
    /// Creates a new category identifier.
    pub fn new<S: Into<String>>(category: S) -> Self {
        Category(category.into())
    }

    /// Creates a group identifier by joining the category with a string.
    pub fn join<S: Into<String>>(&self, group: S) -> Group {
        Group {
            inner: group.into(),
            parent: self.clone(),
        }
    }
}

impl Group {
    /// Creates an index identifier by joining the group with an index.
    pub fn join(&self, index: usize) -> Index {
        Index {
            inner: index,
            parent: self.clone(),
        }
    }

    /// Gets the parent category of this group.
    pub fn parent(&self) -> &Category {
        &self.parent
    }
}

impl Index {
    /// Gets the parent group of this index.
    pub fn parent(&self) -> &Group {
        &self.parent
    }
}

/// A *selector* to filter rules by checking if an [Index] matches the selector.
/// Can be created from a [Category], [Group], or [Index] by casting with `.into()`.
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[serde(try_from = "String", into = "String")]
pub enum Selector {
    /// A category-level selector.
    Category(Category),
    /// A group-level selector.
    Group(Group),
    /// An index-level selector.
    Index(Index),
}

impl From<Category> for Selector {
    fn from(category: Category) -> Self {
        Selector::Category(category)
    }
}

impl From<Group> for Selector {
    fn from(group: Group) -> Self {
        Selector::Group(group)
    }
}

impl From<Index> for Selector {
    fn from(index: Index) -> Self {
        Selector::Index(index)
    }
}

impl TryFrom<&str> for Selector {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value.split('/').collect::<Vec<_>>().as_slice() {
            [category] => Selector::Category(Category::new(*category)),
            [category, group] => Selector::Group(Category::new(*category).join(*group)),
            [category, group, index] => {
                Selector::Index(Category::new(*category).join(*group).join(index.parse()?))
            }
            _ => return Err(Error::ParseStringError(value.to_owned())),
        })
    }
}

impl TryFrom<String> for Selector {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Selector::try_from(value.as_str())
    }
}

impl From<Selector> for String {
    fn from(selector: Selector) -> Self {
        match &selector {
            Selector::Category(x) => format!("{}", x),
            Selector::Group(x) => format!("{}", x),
            Selector::Index(x) => format!("{}", x),
        }
    }
}

impl Selector {
    /// Determines whether an [Index] matches this selector. It maches iff:
    /// 1. the category is the same (if this selector is at category-level)
    /// 2. the group and the category are the same (if this selector is at group-level)
    /// 3. the group and category and index are the same (if this selector is at index-level)
    pub fn is_match(&self, id: &Index) -> bool {
        match &self {
            Selector::Category(category) => id.parent().parent() == category,
            Selector::Group(group) => id.parent() == group,
            Selector::Index(index) => id == index,
        }
    }
}
