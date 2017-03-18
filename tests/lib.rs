#[macro_use]
extern crate html5ever_atoms;
extern crate kuchiki;
extern crate readability;

use kuchiki::{NodeRef, ElementData};
use kuchiki::NodeData::*;
use kuchiki::traits::TendrilSink;
use readability::Readability;


fn compare_trees(actual: NodeRef, expected: NodeRef) {
    let mut actual_it = actual.inclusive_descendants().skip_while(is_empty_text);
    let mut expected_it = expected.inclusive_descendants().skip_while(is_empty_text);

    loop {
        let actual = actual_it.next();
        let expected = expected_it.next();

        match (actual, expected) {
            (None, None) => break,
            (None, Some(node)) => panic!("Expected {}", node.to_string()),
            (Some(node), None) => panic!("Needless {}", node.to_string()),
            (Some(one), Some(two)) => compare_nodes(one, two)
        }
    }
}

fn is_empty_text(node: &NodeRef) -> bool {
    node.as_text().map_or(false, |text| text.borrow().trim().is_empty())
}

fn compare_nodes(actual: NodeRef, expected: NodeRef) {
    let actual_data = actual.data();
    let expected_data = expected.data();

    match (actual_data, expected_data) {
        (&Element(ref actual), &Element(ref expected)) => {
            if actual.name != expected.name {
                panic!("{} != {}", stringify_elem(&actual), stringify_elem(&expected));
            }

            let actual_attrs = &actual.attributes.borrow().map;
            let expected_attrs = &expected.attributes.borrow().map;

            let mut expected_attrs_it = expected_attrs.iter();

            for ((ak, av), (ek, ev)) in actual_attrs.iter().zip(expected_attrs_it.by_ref()) {
                if ak != ek {
                    panic!("{} != {}:\n  Invalid attribute, expected {}",
                           stringify_elem(&actual), stringify_elem(&expected), ek.local);
                }

                if av != ev {
                    panic!("{} != {}:\n  Invalid value of attribute \"{}\", expected {}",
                           stringify_elem(&actual), stringify_elem(&expected), ek.local, ev);
                }
            }

            if let Some((key, value)) = expected_attrs_it.next() {
                panic!("Expected attribute \"{}\"={}", key.local, value);
            }
        },

        (&Text(ref actual), &Text(ref expected)) => {
            let actual = actual.borrow();
            let expected = expected.borrow();

            let actual_words = actual.split_whitespace();
            let expected_words = expected.split_whitespace();

            let eq = actual_words.zip(expected_words).all(|(a, e)| a == e);

            if !eq {
                panic!("TEXT: {} != {}", *actual, *expected);
            }
        },

        (&Comment(_), &Comment(_)) |
        (&Doctype(_), &Doctype(_)) |
        (&Document(_), &Document(_)) |
        (&DocumentFragment, &DocumentFragment) => unimplemented!(),

        _ => panic!("{} != {}", actual.to_string(), expected.to_string())
    };
}

fn stringify_elem(data: &ElementData) -> String {
    let attributes = data.attributes.borrow();
    let classes = attributes.get(local_name!("class"));
    let id = attributes.get(local_name!("id"));

    match (id, classes) {
        (Some(id), Some(classes)) => format!("<{} #{}.{}>", data.name.local, id, classes),
        (Some(id), None) => format!("<{} #{}>", data.name.local, id),
        (None, Some(classes)) => format!("<{} .{}>", data.name.local, classes),
        (None, None) => format!("<{}>", data.name.local)
    }
}

macro_rules! include_sample_file {
    ($name:ident, $file:expr) => {
        include_str!(concat!("../samples/", stringify!($name), "/", $file));
    }
}

macro_rules! test_sample {
    ($name:ident) => {
        #[test]
        fn $name() {
            static SOURCE: &'static str = include_sample_file!($name, "source.html");
            static EXPECTED: &'static str = include_sample_file!($name, "expected.html");

            let actual = Readability::new().parse(SOURCE);
            let expected = kuchiki::parse_html().one(EXPECTED)
                .select("body > *").unwrap().next().unwrap().as_node().clone();
            compare_trees(actual, expected);
        }
    };
}
