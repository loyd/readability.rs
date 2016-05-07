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

struct NodeInfo {
    text_len: usize,
    commas: usize
}

pub struct Readability {
    info: HashMap<HashableNodeRef, NodeInfo>
}

impl Readability {
    pub fn new() -> Readability {
        Readability {
            info: HashMap::new()
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

                NodeEdge::End(node) => {
                    let elem = match node.into_element_ref() {
                        Some(e) => e,
                        None => continue
                    };

                    self.add_info(&elem);

                    if self.is_tag_to_score(&elem.name) {
                        self.score_elem(&elem);
                    }
                }
            }
        }

        self.info.clear();
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

    fn is_tag_to_score(&self, tag: &QualName) -> bool {
        match *tag {
            tag!("section") | tag!("p") | tag!("td") | tag!("pre") |
            tag!("h2") | tag!("h3") | tag!("h4") | tag!("h5") | tag!("h6") => true,
            _ => false
        }
    }

    fn add_info(&mut self, elem: &ElemRef) {
        let mut text_len = 0;
        let mut commas = 0;

        for child in elem.as_node().children() {
            let is_text = child.as_text().map_or(false, |data| {
                let (char_cnt, comma_cnt) = self.count_chars(&data.borrow()[..]);
                text_len += char_cnt;
                commas += comma_cnt;
                true
            });

            if !is_text {
                let key = HashableNodeRef(child);
                let info = &self.info[&key];
                text_len += info.text_len;
                commas += info.commas;
            }
        }

        let key = HashableNodeRef(elem.as_node().clone());
        self.info.insert(key, NodeInfo {
            text_len: text_len,
            commas: commas
        });
    }

    fn count_chars(&self, text: &str) -> (usize, usize) {
        let mut char_cnt = 0;
        let mut comma_cnt = 0;

        //#XXX: what about graphemes?
        let mut iter = text.trim().chars().peekable();

        while let Some(ch) = iter.next() {
            if ch.is_whitespace() {
                if !iter.peek().unwrap().is_whitespace() {
                    char_cnt += 1;
                }
            } else if ch == ',' {
                if iter.peek().map_or(true, |&c| c != ',') {
                    char_cnt += 1;
                    comma_cnt += 1;
                }
            } else {
                char_cnt += 1;
            }
        }

        (char_cnt, comma_cnt)
    }

    fn score_elem(&mut self, elem: &ElemRef) {
        if let None = elem.as_node().parent().and_then(|p| p.into_element_ref()) {
            return;
        }
    }
}

pub fn new() -> Readability {
    Readability::new()
}
