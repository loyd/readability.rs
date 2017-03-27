extern crate env_logger;
extern crate log;
extern crate kuchiki;
extern crate readability;
extern crate url;

use std::env;

use env_logger::LogBuilder;
use kuchiki::NodeRef;
use kuchiki::NodeData::*;
use kuchiki::traits::TendrilSink;
use url::Url;

use readability::Readability;


fn compare_trees(actual: &NodeRef, expected: &NodeRef) {
    compare_nodes(actual, expected);

    let mut actual_it = actual.children().filter(is_not_empty_text);
    let mut expected_it = expected.children().filter(is_not_empty_text);

    loop {
        let actual = actual_it.next();
        let expected = expected_it.next();

        match (actual, expected) {
            (None, None) => break,
            (None, Some(node)) => panic!("Expected {}", stringify_node(&node)),
            (Some(node), None) => panic!("Needless {}", stringify_node(&node)),
            (Some(one), Some(two)) => compare_trees(&one, &two)
        }
    }
}

fn is_not_empty_text(node: &NodeRef) -> bool {
    !node.as_text().map_or(false, |text| text.borrow().trim().is_empty())
}

fn compare_nodes(actual: &NodeRef, expected: &NodeRef) {
    let actual_data = actual.data();
    let expected_data = expected.data();

    match (actual_data, expected_data) {
        (&Element(ref actual_data), &Element(ref expected_data)) => {
            let actual_attributes = &actual_data.attributes.borrow().map;
            let expected_attributes = &expected_data.attributes.borrow().map;

            if actual_data.name != expected_data.name || actual_attributes != expected_attributes {
                panic!("{} != {}", stringify_node(&actual), stringify_node(&expected));
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

        _ => panic!("{} != {}", stringify_node(actual), stringify_node(expected))
    };
}

fn stringify_node(node: &NodeRef) -> String {
    const LIMIT: usize = 40;

    let string = node.to_string();

    match *node.data() {
        Element(_) => {
            let mut pos = 0;

            for slice in string.split_terminator('>') {
                pos += slice.len() + 1;
                if pos >= LIMIT { break; }
            }

            string[..pos].to_owned()
        },
        _ if string.len() > LIMIT => format!("{}...", &string[..LIMIT]),
        _ => string
    }
}

fn setup_logger() {
    let env = match env::var("RUST_LOG") {
        Ok(env) => env,
        Err(_) => return
    };

    let _ = LogBuilder::new()
        .format(|record| format!("{}", record.args()))
        .parse(&env)
        .init();
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

            setup_logger();

            let actual = Readability::new()
                .base_url(Url::parse("http://fakehost/test/page.html").unwrap())
                .parse(SOURCE);

            let expected = kuchiki::parse_html().one(EXPECTED)
                .select("body > *").unwrap().next().unwrap().as_node().clone();

            compare_trees(&actual, &expected);
        }
    };
}

test_sample!(base_url);
test_sample!(social_buttons);

test_sample!(bbc);
test_sample!(cnet);
test_sample!(herald);
test_sample!(libertation);
test_sample!(medium_1);
test_sample!(medium_2);
test_sample!(msn);
test_sample!(nytimes_1);
test_sample!(wikia);
test_sample!(wikipedia);
