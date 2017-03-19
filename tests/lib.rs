extern crate kuchiki;
extern crate readability;

use kuchiki::NodeRef;
use kuchiki::NodeData::*;
use kuchiki::traits::TendrilSink;
use readability::Readability;


fn compare_trees(actual: NodeRef, expected: NodeRef) {
    let mut actual_it = actual.inclusive_descendants().filter(is_not_empty_text);
    let mut expected_it = expected.inclusive_descendants().filter(is_not_empty_text);

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

fn is_not_empty_text(node: &NodeRef) -> bool {
    !node.as_text().map_or(false, |text| text.borrow().trim().is_empty())
}

fn compare_nodes(actual: NodeRef, expected: NodeRef) {
    let actual_data = actual.data();
    let expected_data = expected.data();

    match (actual_data, expected_data) {
        (&Element(ref actual_data), &Element(ref expected_data)) => {
            let actual_attributes = &actual_data.attributes.borrow().map;
            let expected_attributes = &expected_data.attributes.borrow().map;

            if actual_data.name != expected_data.name || actual_attributes != expected_attributes {
                panic!("{} != {}", stringify_elem(&actual), stringify_elem(&expected));
            }
        },

        (&Text(ref actual), &Text(ref expected)) => {
            let actual = actual.borrow();
            let expected = expected.borrow();

            let actual_words = actual.split_whitespace();
            let expected_words = expected.split_whitespace();

            if actual_words.ne(expected_words) {
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

fn stringify_elem(elem: &NodeRef) -> String {
    let string = elem.to_string();
    let mut pos = 0;

    for slice in string.split_terminator('>') {
        pos += slice.len() + 1;
        if pos >= 20 { break; }
    }

    string[..pos].to_owned()
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
