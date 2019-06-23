#[macro_use]

enum Modifier {
  Even,
  Odd,
  EveryOtherEven,
  EveryOtherOdd
}

enum Problems {
  Single { x: u32 },
  Range { x: u32, y: u32 },
  ModifiedRange { x: u32, y: u32, m: Modifier }
}

fn main() {
    println!("Hello, world!");
}
