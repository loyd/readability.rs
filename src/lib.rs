#[macro_use]
extern crate string_cache;
extern crate kuchiki;

use std::collections::HashMap;
use std::ops::Deref;
use std::hash::{Hash, Hasher};

use kuchiki::{Node, NodeRef};
use kuchiki::traits::TendrilSink;
use kuchiki::iter::{NodeEdge, NodeIterator};


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

// Must be sorted.
static UNLIKELY_CANDIDATE: [&'static str; 23] = [
    "ad-break", "agegate", "banner", "combx", "comment", "community", "disqus", "extra", "foot",
    "header", "menu", "modal", "pager", "pagination", "popup", "related", "remark", "rss", "share",
    "shoutbox", "sidebar", "skyscraper", "sponsor"
];

// Must be sorted.
static MAYBE_CANDIDATE: [&'static str; 6] = ["and", "article", "body", "column", "main", "shadow"];

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
                NodeEdge::Start(this) => {
                    self.strip_unlikely_children(this);
                },

                NodeEdge::End(this) => {
                }
            }
        }
    }

    fn strip_unlikely_children(&self, node: NodeRef) {
        for child in node.children().elements() {
            match child.name {
                qualname!(html, "a") | qualname!(html, "body") => continue,
                _ => {}
            }

            let attributes = child.attributes.borrow();

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

            if unlikely {
                child.as_node().detach();
            }
        }
    }
}

pub fn new() -> Readability {
    Readability::new()
}
