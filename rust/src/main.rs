#[macro_use]
use nom::*;

// TODO:
// - `modifier` parser needs to return a `Modifier,` not a `String`
// - Figure out how to return `enum` like `Single` instead of `struct`
// - Figure out how to access fields of `Range` in `modified_range` parser

enum Modifier {
    Even,
    Odd,
    EveryOtherEven,
    EveryOtherOdd,
}

enum Problems {
    Single { x: u32 },
    Range { x: u32, y: u32 },
    ModifiedRange { x: u32, y: u32, m: Modifier },
}

named!(int, take_while1!(is_digit));

// TODO: This needs to return a Modifier, not a String
named!(
    modifier,
    do_parse!(m: alt!(tag!("even") | tag!("odd") | tag!("eoe") | tag!("eoo")) >> (m))
);

named!(single, do_parse!(x: int >> (Single { x })));

named!(
    range,
    do_parse!(x: int >> char!('-') >> y: int >> (Range { x, y }))
);

named!(
    modified_range,
    do_parse!(r: range >> many1!(is_space) >> m: modifier >> (ModifiedRange { r.x, r.y, m }))
);

fn problem_counter(input: &String) -> String {
    let output = input;
    format!("Problems: {}", output)
}

fn main() {
    print!("Enter problems: ");
    let input: String = "foobar".to_string();
    println!("{}", problem_counter(&input));
}
