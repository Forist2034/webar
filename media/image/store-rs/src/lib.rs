pub mod blob {
    pub mod index;
    pub use webar_store::blob as store;
}

pub mod object {
    pub mod index;
    pub use webar_store::object as store;
}
