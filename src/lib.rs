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
use std::f32;

use regex::Regex;
use html5ever_atoms::QualName;
use kuchiki::{Node, NodeRef, NodeDataRef, ElementData};
use kuchiki::traits::TendrilSink;
use kuchiki::iter::NodeIterator;


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
        banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|extra|foot|header|legends|menu|
        modal|related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|
        ad-break|agegate|pagination|pager|popup|yom-remote
    ").unwrap();

    static ref MAYBE_CANDIDATE: Regex = Regex::new(r"(?xi)
        and|article|body|column|main|shadow
    ").unwrap();

    static ref POSITIVE: Regex = Regex::new(r"(?xi)
        article|body|content|entry|hentry|h-entry|main|page|pagination|post|text|blog|story
    ").unwrap();

    static ref NEGATIVE: Regex = Regex::new(r"(?xi)
        hidden|^hid$|\shid$|\shid\s|^hid\s|banner|combx|comment|com-|contact|foot|footer|footnote|
        masthead|media|meta|modal|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|
        sponsor|shopping|tags|tool|widget
    ").unwrap();

    static ref VIDEO: Regex = Regex::new(r"(?xi)
        //(www\.)?(dailymotion|youtube|youtube-nocookie|player\.vimeo)\.com
    ").unwrap();
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

    node.children().text_nodes().all(|t| t.borrow().trim().is_empty())
}

fn has_block_elem(node: &NodeRef) -> bool {
    node.descendants().elements().any(|elem| match elem.name {
        tag!("a") | tag!("blockquote") | tag!("dl") | tag!("div") | tag!("img") | tag!("ol") |
        tag!("p") | tag!("pre") | tag!("table") | tag!("ul") | tag!("select") => true,
        _ => false
    })
}

fn count_chars(text: &str) -> (u32, u32) {
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

fn tag_score(tag: &QualName) -> f32 {
    match *tag {
        tag!("article") => 30.,
        tag!("section") => 15.,
        tag!("div") => 5.,
        tag!("pre") | tag!("td") | tag!("blockquote") => 3.,
        tag!("address") | tag!("form") => -3.,
        tag!("dl") | tag!("dt") | tag!("dd") => -3.,
        tag!("li") | tag!("ol") | tag!("ul") => -3.,
        tag!("body") => -5.,
        tag!("h2") | tag!("h3") | tag!("h4") | tag!("h5") | tag!("h6") | tag!("th") => -5.,
        _ => 0.
    }
}

fn class_score(elem: &ElemRef) -> f32 {
    let attributes = elem.attributes.borrow();
    let mut score = 0.;

    if let Some(classes) = attributes.get(attrib!("class")) {
        if POSITIVE.is_match(classes) { score += 25.; }
        if NEGATIVE.is_match(classes) { score -= 25.; }
    }

    if let Some(id) = attributes.get(attrib!("id")) {
        if POSITIVE.is_match(id) { score += 25.; }
        if NEGATIVE.is_match(id) { score -= 25.; }
    }

    score
}

fn is_stuffed(elem: &ElemRef, info: &NodeInfo) -> bool {
    match elem.name {
        tag!("blockquote") | tag!("li") | tag!("p") | tag!("pre") |
        tag!("thead") | tag!("tbody") | tag!("th") | tag!("tr") | tag!("td") => {},
        _ => return true
    };

    //#TODO: add <video>, <audio> and <iframe> counters to the sum.
    info.text_len > 0 || info.img_count + info.embed_count > 0
}

fn is_conditionally_acceptable(elem: &ElemRef, info: &NodeInfo) -> bool {
    let is_list = match elem.name {
        tag!("form") | tag!("fieldset") | tag!("table") | tag!("div") => false,
        tag!("ul") | tag!("ol") => true,
        _ => return true
    };

    //#TODO: cache the score to prevent extra calculations.
    let class_score = class_score(elem);

    if class_score < 0. {
        return false;
    }

    if info.commas >= 10 {
        return true;
    }

    let link_density = info.link_len as f32 / info.text_len as f32;
    let p_img_ratio = info.p_count as f32 / info.img_count as f32;

    //#TODO: take into account ancestor tags (check "figure").
    !(
        (info.img_count > 1 && p_img_ratio < 0.5) ||
        (!is_list && info.li_count > info.p_count + 100) ||
        (info.input_count > info.p_count / 3) ||
        (!is_list && info.text_len < 25 && (info.img_count == 0 || info.img_count > 2)) ||
        (!is_list && class_score < 25. && link_density > 0.2) ||
        (class_score >= 25. && link_density > 0.5) ||
        ((info.embed_count == 1 && info.text_len < 75) || info.embed_count > 1)
     )
}

#[derive(Debug, Default, PartialEq)]
struct NodeInfo {
    content_score: f32,
    text_len: u32,
    link_len: u32,
    commas: u32,

    p_count: u32,
    img_count: u32,
    li_count: u32,
    input_count: u32,
    embed_count: u32,
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
        let mut current = top_level.clone();
        let mut bubbling = false;

        loop {
            if !bubbling {
                self.on_capturing(&current);

                if let Some(child) = current.first_child() {
                    current = child;
                    continue;
                }

                bubbling = true;
            }

            let (ncurrent, nbubbling) = if let Some(next) = current.next_sibling() {
                (next, false)
            } else if let Some(parent) = current.parent() {
                (parent, bubbling)
            } else {
                if bubbling { self.on_bubbling(&current); }
                break;
            };

            if bubbling { self.on_bubbling(&current); }

            bubbling = nbubbling;
            current = ncurrent;
        }

        let best = self.select_best();

        //#TODO: add something more clever.
        //#TODO: don't forget about urls.
        best.map_or(top_level, |b| b.as_node().clone())
    }

    // Capturing stage: remove unlikely candidates, unpack divs etc.
    fn on_capturing(&mut self, node: &NodeRef) {
        for child in node.children().elements() {
            if is_unlikely_candidate(&child) {
                child.remove();
            } else if child.is(tag!("div")) {
                unpack_div_if_needed(&child);
            }
        }
    }

    // Bubbling stage: collect info based on children and score elements.
    fn on_bubbling(&mut self, node: &NodeRef) {
        let tag = match node.as_element() {
            Some(&ElementData { ref name, .. }) => name,
            None => return
        };

        let acceptable = {
            let info = self.add_info(&node);
            let elem = node.clone().into_element_ref().unwrap();

            is_stuffed(&elem, info) && is_conditionally_acceptable(&elem, info)
        };

        if is_tag_to_score(tag) {
            self.score_node(&node);
        }

        //#XXX: maybe it should be before the score propagation?
        if !acceptable {
            node.remove();
        }
    }

    fn add_info(&mut self, node: &NodeRef) -> &NodeInfo {
        let mut text_len = 0;
        let mut link_len = 0;
        let mut commas = 0;

        let mut p_count = 0;
        let mut img_count = 0;
        let mut li_count = 0;
        let mut input_count = 0;
        let mut embed_count = 0;

        for child in node.children() {
            if let Some(data) = child.as_text() {
                let (char_cnt, comma_cnt) = count_chars(&data.borrow()[..]);
                text_len += char_cnt;
                commas += comma_cnt;
                continue;
            };

            if let Some(elem) = child.as_element() {
                match elem.name {
                    tag!("p") => p_count += 1,
                    tag!("img") => img_count += 1,
                    tag!("li") => li_count += 1,
                    tag!("input") => input_count += 1,
                    tag!("embed") => {
                        let attribs = elem.attributes.borrow();
                        let src = attribs.get(attrib!("src")).unwrap_or("");

                        if !VIDEO.is_match(src) {
                            embed_count += 1;
                        }
                    },
                    _ => {}
                };
            } else {
                continue;
            }

            let is_a = child.is(tag!("a"));
            let key = HashableNodeRef(child);
            let info = &self.info[&key];

            link_len += if is_a { info.text_len } else { info.link_len };
            text_len += info.text_len;
            commas += info.commas;
            p_count += info.p_count;
            img_count += info.img_count;
            li_count += info.li_count;
            input_count += info.input_count;
            embed_count += info.embed_count;
        }

        let key = HashableNodeRef(node.clone());
        let info = self.info.entry(key).or_insert_with(Default::default);

        debug_assert_eq!(info, &NodeInfo {
            content_score: info.content_score,
            ..Default::default()
        });

        info.text_len = text_len;
        info.link_len = link_len;
        info.commas = commas;
        info.p_count = p_count;
        info.img_count = img_count;
        info.li_count = li_count;
        info.input_count = input_count;
        info.embed_count = embed_count;

        info
    }

    fn score_node(&mut self, node: &NodeRef) {
        if let Some(content_score) = self.calculate_content_score(node) {
            self.propagate_score(node, content_score);
        }
    }

    fn calculate_content_score(&self, node: &NodeRef) -> Option<f32> {
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

        Some(content_score as f32)
    }

    fn propagate_score(&mut self, node: &NodeRef, content_score: f32) {
        for (level, ancestor) in node.ancestors().elements().enumerate() {
            if is_tag_to_score(&ancestor.name) {
                return;
            }

            let div = match level {
                0 => 1.,
                1 => 2.,
                _ => 3. * level as f32
            };

            let addition = content_score / div;

            let key = HashableNodeRef(ancestor.as_node().clone());
            let mut first_meeting = false;

            let info = self.info.entry(key).or_insert_with(|| {
                first_meeting = true;
                Default::default()
            });

            info.content_score += addition;

            if first_meeting {
                self.candidates.push(ancestor);
            }
        }
    }

    fn select_best(&mut self) -> Option<ElemRef> {
        let mut best = None;
        let mut best_score = -f32::INFINITY;

        for candidate in self.candidates.drain(..) {
            let key = HashableNodeRef(candidate.as_node().clone());
            let info = &self.info[&key];

            debug_assert!(info.text_len > 0);
            debug_assert!(info.text_len >= info.link_len);

            // Add content points.
            let mut score = info.content_score;

            // Add points for tag name.
            score += tag_score(&candidate.name);

            // Add points for an class/id weight.
            score += class_score(&candidate);

            // Scale the final score based on link density. Good content should have a relatively
            // small link density (5% or less) and be mostly unaffected by this operation.
            score *= 1. - info.link_len as f32 / info.text_len as f32;

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
