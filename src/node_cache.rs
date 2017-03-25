use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use kuchiki::{Node, NodeRef};


#[derive(PartialEq, Eq)]
struct HashableNodeRef(NodeRef);

impl Hash for HashableNodeRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Node = &*(self.0).0;
        state.write_usize(ptr as usize);
    }
}

pub struct NodeCache<T>(HashMap<HashableNodeRef, T>);

impl<T: Default> NodeCache<T> {
    pub fn new() -> NodeCache<T> {
        NodeCache(HashMap::new())
    }

    pub fn get(&mut self, node: &NodeRef) -> &mut T {
        let key = HashableNodeRef(node.clone());
        self.0.entry(key).or_insert_with(Default::default)
    }

    pub fn get_has(&mut self, node: &NodeRef) -> (&mut T, bool) {
        let key = HashableNodeRef(node.clone());
        let mut stored = true;

        let entry = self.0.entry(key).or_insert_with(|| {
            stored = false;
            Default::default()
        });

        (entry, stored)
    }
}
