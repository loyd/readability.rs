#[macro_use]
extern crate string_cache;
extern crate kuchiki;

use std::collections::HashMap;
use std::ops::Deref;
use std::hash::{Hash, Hasher};

use string_cache::QualName;
use kuchiki::{Node, NodeRef, NodeDataRef, ElementData};
use kuchiki::traits::TendrilSink;
use kuchiki::iter::{NodeEdge, NodeIterator};


type ElemRef = NodeDataRef<ElementData>;

#[derive(PartialEq, Eq)]
struct HashableNodeRef(NodeRef);

impl Deref for HashableNodeRef {
    type Target = NodeRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Hash for HashableNodeRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Node = &*(self.0).0;
        state.write_usize(ptr as usize);
    }
}

trait NodeRefExt {
    fn node_ref(&self) -> &NodeRef;

    fn is(&self, name: QualName) -> bool {
        self.node_ref().as_element().map_or(false, |e| e.name == name)
    }

    fn replace<N: NodeRefExt>(&self, node: N) {
        self.node_ref().insert_before(node.node_ref().clone());
        self.remove();
    }

    fn remove(&self) {
        self.node_ref().detach();
    }

    fn rename(&self, name: QualName) {
        let node = self.node_ref();

        if let Some(elem) = node.as_element() {
            let mut attributes = elem.attributes.borrow_mut();
            let replacement = NodeRef::new_element(name, attributes.map.drain());

            for child in node.children() {
                replacement.append(child);
            }

            node.replace(replacement);
        }
    }
}

impl NodeRefExt for NodeRef {
    #[inline]
    fn node_ref(&self) -> &NodeRef {
        self
    }
}

impl NodeRefExt for ElemRef {
    #[inline]
    fn node_ref(&self) -> &NodeRef {
        self.as_node()
    }
}

// Must be sorted.
static UNLIKELY_CANDIDATE: [&'static str; 23] = [
    "ad-break", "agegate", "banner", "combx", "comment", "community", "disqus", "extra", "foot",
    "header", "menu", "modal", "pager", "pagination", "popup", "related", "remark", "rss", "share",
    "shoutbox", "sidebar", "skyscraper", "sponsor"
];

// Must be sorted.
static MAYBE_CANDIDATE: [&'static str; 6] = ["and", "article", "body", "column", "main", "shadow"];

macro_rules! tag {
    ($name:tt) => { qualname!(html, $name) };
}

pub struct Readability {
    scores: HashMap<HashableNodeRef, u32>
}

impl Readability {
    pub fn new() -> Readability {
        Readability {
            scores: HashMap::new()
        }
    }

    pub fn parse(&mut self, html: &str) -> NodeRef {
        let top_level = kuchiki::parse_html().one(html);
        self.readify(top_level.clone());
        top_level
    }

    fn readify(&mut self, top_level: NodeRef) {
        for edge in top_level.traverse() {
            match edge {
                NodeEdge::Start(node) => {
                    for child in node.children().elements() {
                        if self.is_unlikely_candidate(&child) {
                            child.remove();
                        } else if child.is(tag!("div")) {
                            self.convert_div_if_needed(&child);
                        }
                    }
                },

                NodeEdge::End(node) => {}
            }
        }
    }

    fn is_unlikely_candidate(&self, elem: &ElemRef) -> bool {
        match elem.name {
            tag!("a") | tag!("body") => return false,
            _ => {}
        }

        let attributes = elem.attributes.borrow();

        // Ok, check classes.
        let mut unlikely = false;

        let classes = attributes.get(atom!("class")).unwrap_or("");
        let id = attributes.get(atom!("id")).unwrap_or("");

        let class_it = classes
            .split_whitespace()
            .map(|c| c.to_lowercase());

        let id_it = id
            .split_whitespace()
            .map(|c| c.to_lowercase());

        for word in class_it.chain(id_it) {
            if MAYBE_CANDIDATE.binary_search(&&word[..]).is_ok() {
                unlikely = false;
                break;
            }

            if !unlikely && UNLIKELY_CANDIDATE.binary_search(&&word[..]).is_ok() {
                unlikely = true;
            }
        }

        return unlikely;
    }

    fn convert_div_if_needed(&self, div: &ElemRef) {
        let node = div.as_node();

        if self.has_single_p(node) {
            let p = node.children().elements().next().unwrap();
            node.replace(p);
        } else if !self.has_block_elem(node) {
            node.rename(tag!("p"));
        }
    }

    fn has_single_p(&self, node: &NodeRef) -> bool {
        let mut child_it = node.children().elements();
        let child = child_it.next();

        if child.is_none() || child_it.next().is_some() {
            return false;
        }

        let child = child.unwrap();
        if !child.is(tag!("p")) {
            return false;
        }

        let no_text = node.children().text_nodes().all(|t| t.borrow().trim().is_empty());
        return no_text;
    }

    fn has_block_elem(&self, node: &NodeRef) -> bool {
        node.descendants().elements().any(|elem| match elem.name {
            tag!("a") | tag!("blockquote") | tag!("dl") | tag!("div") | tag!("img") | tag!("ol") |
            tag!("p") | tag!("pre") | tag!("table") | tag!("ul") | tag!("select") => true,
            _ => false
        })
    }
}

pub fn new() -> Readability {
    Readability::new()
}
