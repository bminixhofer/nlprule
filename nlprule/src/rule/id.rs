use serde::{Deserialize, Serialize};
use std::{convert::TryFrom, fmt, num::ParseIntError};

#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
    #[error("error parsing selector from string: {0}")]
    ParseStringError(String),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
pub struct Category(String);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
pub struct Group {
    inner: String,
    parent: Category,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, Default)]
pub struct Index {
    inner: usize,
    parent: Group,
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
    pub fn new<S: Into<String>>(category: S) -> Self {
        Category(category.into())
    }

    pub fn join<S: Into<String>>(&self, group: S) -> Group {
        Group {
            inner: group.into(),
            parent: self.clone(),
        }
    }
}

impl Group {
    pub fn join(&self, index: usize) -> Index {
        Index {
            inner: index,
            parent: self.clone(),
        }
    }

    pub fn parent(&self) -> &Category {
        &self.parent
    }
}

impl Index {
    pub fn parent(&self) -> &Group {
        &self.parent
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, Eq)]
#[serde(try_from = "String", into = "String")]
pub enum Selector {
    Category(Category),
    Group(Group),
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

impl TryFrom<String> for Selector {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(match value.split('/').collect::<Vec<_>>().as_slice() {
            [category] => Selector::Category(Category::new(*category)),
            [category, group] => Selector::Group(Category::new(*category).join(*group)),
            [category, group, index] => {
                Selector::Index(Category::new(*category).join(*group).join(index.parse()?))
            }
            _ => return Err(Error::ParseStringError(value.into())),
        })
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
    pub fn is_match(&self, id: &Index) -> bool {
        match &self {
            Selector::Category(category) => id.parent().parent() == category,
            Selector::Group(group) => id.parent() == group,
            Selector::Index(index) => id == index,
        }
    }
}
