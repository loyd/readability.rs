#[macro_use]
extern crate html5ever_atoms;
#[macro_use]
extern crate lazy_static;
extern crate kuchiki;
extern crate regex;

use std::collections::HashMap;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::cmp;
use std::default::Default;
use std::isize;

use regex::Regex;
use html5ever_atoms::QualName;
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

lazy_static! {
    static ref UNLIKELY_CANDIDATE: Regex = Regex::new(r"(?xi)
        ad-break|agegate|auth?or|bookmark|cat|com(?:bx|ment|munity)|date|disqus|extra|foot|header|
        ignore|links|menu|nav|pag(?:er|ination)|popup|related|remark|rss|share|shoutbox|sidebar|
        similar|social|sponsor|teaserlist|time|tweet|twitter
    ").unwrap();

    static ref MAYBE_CANDIDATE: Regex = Regex::new(r"(?xi)
        and|article|body|column|main|shadow
    ").unwrap();

    static ref POSITIVE: Regex = Regex::new(r"(?xi)
        article|blog|body|content|entry|main|news|pag(?:e|ination)|post|story|text
    ").unwrap();

    static ref NEGATIVE: Regex = Regex::new(r"(?xi
        com(?:bx|ment|-)|contact|foot(?:er|note)?|masthead|media|meta|outbrain|promo|related|
        scroll|shoutbox|sidebar|sponsor|shopping|tags|tool|widget|^hid$|\shid\s|^hid\s|hidden
    )").unwrap();
}

macro_rules! tag {
    ($name:tt) => { qualname!(html, $name) };
}

macro_rules! attrib {
    ($name:tt) => { local_name!($name) };
}

fn is_unlikely_candidate(elem: &ElemRef) -> bool {
    match elem.name {
        tag!("a") | tag!("body") => return false,
        _ => {}
    }

    let attributes = elem.attributes.borrow();

    // Ok, check classes.
    let classes = attributes.get(attrib!("class")).unwrap_or("");
    let id = attributes.get(attrib!("id")).unwrap_or("");

    (UNLIKELY_CANDIDATE.is_match(classes) || UNLIKELY_CANDIDATE.is_match(id)) &&
        !(MAYBE_CANDIDATE.is_match(classes) || MAYBE_CANDIDATE.is_match(id))
}

fn unpack_div_if_needed(div: &ElemRef) {
    debug_assert_eq!(div.name, tag!("div"));

    let node = div.as_node();

    if has_single_p(node) {
        let p = node.children().elements().next().unwrap();
        node.replace(p);
    } else if !has_block_elem(node) {
        node.rename(tag!("p"));
    }
}

fn has_single_p(node: &NodeRef) -> bool {
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

fn has_block_elem(node: &NodeRef) -> bool {
    node.descendants().elements().any(|elem| match elem.name {
        tag!("a") | tag!("blockquote") | tag!("dl") | tag!("div") | tag!("img") | tag!("ol") |
        tag!("p") | tag!("pre") | tag!("table") | tag!("ul") | tag!("select") => true,
        _ => false
    })
}

fn count_chars(text: &str) -> (usize, usize) {
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

fn is_tag_to_score(tag: &QualName) -> bool {
    match *tag {
        tag!("section") | tag!("p") | tag!("td") | tag!("pre") |
        tag!("h2") | tag!("h3") | tag!("h4") | tag!("h5") | tag!("h6") => true,
        _ => false
    }
}

fn tag_score(tag: &QualName) -> isize {
    match *tag {
        tag!("article") => 30,
        tag!("section") => 15,
        tag!("div") => 5,
        tag!("pre") | tag!("td") | tag!("blockquote") => 3,
        tag!("address") | tag!("form") => -3,
        tag!("dl") | tag!("dt") | tag!("dd") => -3,
        tag!("li") | tag!("ol") | tag!("ul") => -3,
        tag!("body") => -5,
        tag!("h2") | tag!("h3") | tag!("h4") | tag!("h5") | tag!("h6") | tag!("th") => -5,
        _ => 0
    }
}

fn class_score(elem: &ElemRef) -> isize {
    let attributes = elem.attributes.borrow();
    let mut score = 0;

    if let Some(classes) = attributes.get(attrib!("class")) {
        if POSITIVE.is_match(classes) { score += 25; }
        if NEGATIVE.is_match(classes) { score -= 25; }
    }

    if let Some(id) = attributes.get(attrib!("id")) {
        if POSITIVE.is_match(id) { score += 25; }
        if NEGATIVE.is_match(id) { score -= 25; }
    }

    score
}

#[derive(Default)]
struct NodeInfo {
    content_score: usize,
    text_len: usize,
    link_len: usize,
    commas: usize
}

pub struct Readability {
    info: HashMap<HashableNodeRef, NodeInfo>,
    candidates: Vec<ElemRef>
}

impl Readability {
    pub fn new() -> Readability {
        Readability {
            info: HashMap::new(),
            candidates: Vec::new()
        }
    }

    pub fn parse(&mut self, html: &str) -> NodeRef {
        let top_level = kuchiki::parse_html().one(html);
        self.readify(top_level.clone())
    }

    fn readify(&mut self, top_level: NodeRef) -> NodeRef {
        for edge in top_level.traverse() {
            match edge {
                /*
                 * Capturing stage.
                 * Remove unlikely candidates, unpack divs etc.
                 */
                NodeEdge::Start(node) => {
                    for child in node.children().elements() {
                        if is_unlikely_candidate(&child) {
                            child.remove();
                        } else if child.is(tag!("div")) {
                            unpack_div_if_needed(&child);
                        }
                    }
                },

                /*
                 * Bubbling stage.
                 * Collect info based on children and score elements.
                 */
                NodeEdge::End(node) => {
                    let tag_name = match node.as_element() {
                        Some(&ElementData { ref name, .. }) => name,
                        None => continue
                    };

                    self.add_info(&node);

                    if is_tag_to_score(tag_name) {
                        self.score_node(&node);
                    }
                }
            }
        }

        /*
         * Selecting stage.
         */

        let best = self.select_best();

        //#TODO: add something more clever.
        best.map_or(top_level, |b| b.as_node().clone())
    }

    fn add_info(&mut self, node: &NodeRef) {
        let mut text_len = 0;
        let mut link_len = 0;
        let mut commas = 0;

        for child in node.children() {
            let is_text = child.as_text().map_or(false, |data| {
                let (char_cnt, comma_cnt) = count_chars(&data.borrow()[..]);
                text_len += char_cnt;
                commas += comma_cnt;
                true
            });

            if !is_text {
                let is_a = child.is(tag!("a"));
                let key = HashableNodeRef(child);
                let info = &self.info[&key];
                commas += info.commas;

                if is_a {
                    link_len += info.text_len + info.link_len;
                } else {
                    link_len += info.link_len;
                    text_len += info.text_len;
                }
            }
        }

        let key = HashableNodeRef(node.clone());
        let info = self.info.entry(key).or_insert_with(Default::default);

        debug_assert_eq!(info.text_len, 0);
        debug_assert_eq!(info.link_len, 0);
        debug_assert_eq!(info.commas, 0);

        info.text_len = text_len;
        info.link_len = link_len;
        info.commas = commas;
    }

    fn score_node(&mut self, node: &NodeRef) {
        if let Some(content_score) = self.calculate_content_score(node) {
            self.propagate_score(node, content_score);
        }
    }

    fn calculate_content_score(&self, node: &NodeRef) -> Option<usize> {
        let parent_elem = node.parent().and_then(|p| p.into_element_ref());

        if parent_elem.is_none() {
            return None;
        }

        let key = HashableNodeRef(node.clone());
        let info = &self.info[&key];

        if info.text_len < 25 {
            return None;
        }

        // Add a point for the paragraph itself as a base.
        let mut content_score = 1;

        // Add points for any commas within this paragraph.
        content_score += info.commas;

        // For every 100 characters in this paragraph, add another point. Up to 3 points.
        let total_len = info.text_len + info.link_len;
        content_score += cmp::min(total_len / 100, 3);

        Some(content_score)
    }

    fn propagate_score(&mut self, node: &NodeRef, content_score: usize) {
        for (level, ancestor) in node.ancestors().elements().enumerate() {
            if is_tag_to_score(&ancestor.name) {
                return;
            }

            let key = HashableNodeRef(ancestor.as_node().clone());
            let first_meeting = !self.info.contains_key(&key);

            let div = match level {
                0 => 1,
                1 => 2,
                _ => level * 3
            };

            let addition = content_score / div;

            let info = self.info.entry(key).or_insert_with(Default::default);
            info.content_score += addition;

            if first_meeting {
                self.candidates.push(ancestor);
            }
        }
    }

    fn select_best(&mut self) -> Option<ElemRef> {
        let mut best = None;
        let mut best_score = isize::MIN;

        for candidate in self.candidates.drain(..) {
            let key = HashableNodeRef(candidate.as_node().clone());
            let info = &self.info[&key];

            debug_assert!(info.text_len > 0);
            debug_assert!(info.text_len >= info.link_len);

            // Add content points.
            let mut score = info.content_score as isize;

            // Add points for tag name.
            score += tag_score(&candidate.name);

            // Add points for an class/id weight.
            score += class_score(&candidate);

            // Scale the final score based on link density. Good content should have a relatively
            // small link density (5% or less) and be mostly unaffected by this operation.
            score *= ((info.text_len - info.link_len) / info.text_len) as isize;

            if score > best_score {
                best = Some(candidate);
                best_score = score;
            }
        }

        self.info.clear();
        best
    }
}

pub fn new() -> Readability {
    Readability::new()
}
