use std::fmt::Display;

use serde::Deserialize;

use webar_core::object::ObjectId;
use webar_data::ser::{Never, Serialize};

use crate::rest_api::ApiObjectType;

// stub definition
pub struct FilterInfo(Never);

pub type FilterId = ObjectId<FilterInfo>;

#[derive(Debug, Serialize)]
pub struct FilterSpec<I> {
    pub name: I,
    pub id: FilterId,
}

#[derive(Debug)]
pub struct TypeMapError<E> {
    pub field: ApiObjectType,
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
        ApiObjectType::Answer
    };
    (badge) => {
        ApiObjectType::Badge
    };
    (comment) => {
        ApiObjectType::Comment
    };
    (collective) => {
        ApiObjectType::Collective
    };
    (info) => {
        ApiObjectType::Info
    };
    (question) => {
        ApiObjectType::Question
    };
    (revision) => {
        ApiObjectType::Revision
    };
    (tag) => {
        ApiObjectType::Tag
    };
    (tag_synonym) => {
        ApiObjectType::TagSynonym
    };
    (tag_wiki) => {
        ApiObjectType::TagWiki
    };
    (user) => {
        ApiObjectType::User
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
        f: impl Fn(ApiObjectType, &'a N) -> Result<(), E>,
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
impl<N> std::ops::Index<ApiObjectType> for TypeMap<N> {
    type Output = N;
    #[inline]
    fn index(&self, index: ApiObjectType) -> &Self::Output {
        match index {
            ApiObjectType::Answer => &self.answer,
            ApiObjectType::Badge => &self.badge,
            ApiObjectType::Collective => &self.collective,
            ApiObjectType::Comment => &self.comment,
            ApiObjectType::Info => &self.info,
            ApiObjectType::Question => &self.question,
            ApiObjectType::Revision => &self.revision,
            // ObjectType::Site => &self.site,
            ApiObjectType::Tag => &self.tag,
            ApiObjectType::TagSynonym => &self.tag_synonym,
            ApiObjectType::TagWiki => &self.tag_wiki,
            ApiObjectType::User => &self.user,
        }
    }
}
impl<N> std::ops::IndexMut<ApiObjectType> for TypeMap<N> {
    #[inline]
    fn index_mut(&mut self, index: ApiObjectType) -> &mut Self::Output {
        match index {
            ApiObjectType::Answer => &mut self.answer,
            ApiObjectType::Badge => &mut self.badge,
            ApiObjectType::Collective => &mut self.collective,
            ApiObjectType::Comment => &mut self.comment,
            ApiObjectType::Info => &mut self.info,
            ApiObjectType::Question => &mut self.question,
            ApiObjectType::Revision => &mut self.revision,
            // ObjectType::Site => &mut self.site,
            ApiObjectType::Tag => &mut self.tag,
            ApiObjectType::TagSynonym => &mut self.tag_synonym,
            ApiObjectType::TagWiki => &mut self.tag_wiki,
            ApiObjectType::User => &mut self.user,
        }
    }
}
