use std::fmt::Display;

use serde::Deserialize;

use webar_data::ser::Serialize;

use crate::ObjectType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
#[serde(transparent)]
pub struct FilterId(pub webar_core::digest::Digest);
impl Serialize for FilterId {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

#[derive(Debug, Serialize)]
pub struct FilterInfo<I> {
    pub name: I,
    pub id: FilterId,
}

#[derive(Debug)]
pub struct TypeMapError<E> {
    pub field: ObjectType,
    pub error: E,
}
impl<E> Display for TypeMapError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "failed to process field {:?}", self.field)
    }
}
impl<E: std::error::Error + 'static> std::error::Error for TypeMapError<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeMap<N> {
    pub answer: N,
    pub badge: N,
    pub comment: N,
    pub collective: N,
    pub info: N,
    pub question: N,
    pub revision: N,
    pub tag: N,
    pub tag_synonym: N,
    pub tag_wiki: N,
    pub user: N,
    // pub site: N,
}
macro_rules! each_field {
    ($($field:ident),+) => {
        TypeMap {
            $($field: field!($field)),+
        }
    };
    () => {
        each_field!(
            answer,
            badge,
            comment,
            collective,
            info,
            question,
            revision,
            tag,
            tag_synonym,
            tag_wiki,
            user
            // ,site
        )
    }
}
macro_rules! field_ty {
    (answer) => {
        ObjectType::Answer
    };
    (badge) => {
        ObjectType::Badge
    };
    (comment) => {
        ObjectType::Comment
    };
    (collective) => {
        ObjectType::Collective
    };
    (info) => {
        ObjectType::Info
    };
    (question) => {
        ObjectType::Question
    };
    (revision) => {
        ObjectType::Revision
    };
    (tag) => {
        ObjectType::Tag
    };
    (tag_synonym) => {
        ObjectType::TagSynonym
    };
    (tag_wiki) => {
        ObjectType::TagWiki
    };
    (user) => {
        ObjectType::User
    }; // (site) => {
       // ObjectType::Site
       // };
}
impl<N> TypeMap<N> {
    pub fn map<T>(self, f: impl Fn(N) -> T) -> TypeMap<T> {
        macro_rules! field {
            ($f:ident) => {
                f(self.$f)
            };
        }
        each_field!()
    }
    pub fn try_map_ref<'a, T, E>(
        &'a self,
        f: impl Fn(&'a N) -> Result<T, E>,
    ) -> Result<TypeMap<T>, TypeMapError<E>> {
        macro_rules! field {
            ($f:ident) => {
                f(&self.$f).map_err(|e| TypeMapError {
                    field: field_ty!($f),
                    error: e,
                })?
            };
        }
        Ok(each_field!())
    }
    pub fn map_ref<'a, T>(&'a self, f: impl Fn(&'a N) -> T) -> TypeMap<T> {
        macro_rules! field {
            ($f:ident) => {
                f(&self.$f)
            };
        }
        each_field!()
    }
    pub fn try_for_each<'a, E>(
        &'a self,
        f: impl Fn(&'a N) -> Result<(), E>,
    ) -> Result<(), TypeMapError<E>> {
        self.try_map_ref(f).map(|_| ())
    }
    pub fn try_for_each_field<'a, E>(
        &'a self,
        f: impl Fn(ObjectType, &'a N) -> Result<(), E>,
    ) -> Result<(), TypeMapError<E>> {
        macro_rules! field {
            ($field:ident) => {
                f(field_ty!($field), &self.$field).map_err(|e| TypeMapError {
                    field: field_ty!($field),
                    error: e,
                })?
            };
        }
        each_field!();
        Ok(())
    }
}
impl<N> std::ops::Index<ObjectType> for TypeMap<N> {
    type Output = N;
    #[inline]
    fn index(&self, index: ObjectType) -> &Self::Output {
        match index {
            ObjectType::Answer => &self.answer,
            ObjectType::Badge => &self.badge,
            ObjectType::Collective => &self.collective,
            ObjectType::Comment => &self.comment,
            ObjectType::Info => &self.info,
            ObjectType::Question => &self.question,
            ObjectType::Revision => &self.revision,
            // ObjectType::Site => &self.site,
            ObjectType::Tag => &self.tag,
            ObjectType::TagSynonym => &self.tag_synonym,
            ObjectType::TagWiki => &self.tag_wiki,
            ObjectType::User => &self.user,
        }
    }
}
impl<N> std::ops::IndexMut<ObjectType> for TypeMap<N> {
    #[inline]
    fn index_mut(&mut self, index: ObjectType) -> &mut Self::Output {
        match index {
            ObjectType::Answer => &mut self.answer,
            ObjectType::Badge => &mut self.badge,
            ObjectType::Collective => &mut self.collective,
            ObjectType::Comment => &mut self.comment,
            ObjectType::Info => &mut self.info,
            ObjectType::Question => &mut self.question,
            ObjectType::Revision => &mut self.revision,
            // ObjectType::Site => &mut self.site,
            ObjectType::Tag => &mut self.tag,
            ObjectType::TagSynonym => &mut self.tag_synonym,
            ObjectType::TagWiki => &mut self.tag_wiki,
            ObjectType::User => &mut self.user,
        }
    }
}
