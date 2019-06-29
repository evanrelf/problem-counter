// #[macro_use]
use nom::{
    branch::alt, bytes::complete::tag, bytes::complete::take_while1, character::is_digit,
    combinator::map_res, IResult,
};

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

fn int(input: &str) -> IResult<&str, u32> {
    map_res(take_while1(is_digit) /* TODO */)(input)
}

fn modifier(input: &str) -> IResult<&str, Modifier> {
    let m = alt((tag("even"), tag("odd"), tag("eoe"), tag("eoo")))(input);
    match m {
        "even" => Ok((input, Even)),
        "odd" => Ok((input, Odd)),
        "eoe" => Ok((input, EveryOtherEven)),
        "eoo" => Ok((input, EveryOtherOdd)),
        _ => Err("Foobar"),
    }
}

fn single(input: &str) -> IResult<&str, Problems> {
    let x = int()(input)?;
    Ok((input, Single { x }))
}

fn range(input: &str) -> IResult<&str, Problems> {}

fn modified_range(input: &str) -> IResult<&str, Problems> {}

// named!(single, do_parse!(x: int >> (Single { x })));
//
// named!(
//     range,
//     do_parse!(x: int >> char!('-') >> y: int >> (Range { x, y }))
// );
//
// named!(
//     modified_range,
//     do_parse!(r: range >> many1!(is_space) >> m: modifier >> (ModifiedRange { r.x, r.y, m }))
// );

fn problem_counter(input: &String) -> String {
    let output = input;
    format!("Problems: {}", output)
}

fn main() {
    print!("Enter problems: ");
    let input: String = "foobar".to_string();
    println!("{}", problem_counter(&input));
}
