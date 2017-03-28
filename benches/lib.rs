#![feature(test)]

extern crate readability;
extern crate url;

extern crate test;

use test::Bencher;
use url::Url;

use readability::Readability;


macro_rules! include_sample_file {
    ($name:ident, $file:expr) => {
        include_str!(concat!("../samples/", stringify!($name), "/", $file));
    }
}

macro_rules! bench_sample {
    ($name:ident) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            static SOURCE: &'static str = include_sample_file!($name, "source.html");

            b.iter(||
                Readability::new()
                    .base_url(Url::parse("http://fakehost/test/page.html").unwrap())
                    .parse(SOURCE));
        }
    };
}

bench_sample!(bbc);
bench_sample!(buzzfeed);
bench_sample!(cnet);
bench_sample!(ehow_2);
bench_sample!(heise);
bench_sample!(herald_sun);
bench_sample!(iab);
bench_sample!(libertation);
bench_sample!(medium_1);
bench_sample!(medium_2);
bench_sample!(msn);
bench_sample!(nytimes_1);
bench_sample!(wikia);
bench_sample!(wikipedia);
bench_sample!(wordpress);
