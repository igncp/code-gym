/*

https://adventofcode.com/2018/day/3

--- Day 3: No Matter How You Slice It ---

The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to
someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night).
Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the fabric.

The whole piece of fabric they're working on is a very large square - at least 1000 inches on each
side.

Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims
have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each
claim's rectangle is defined as follows:

The number of inches between the left edge of the fabric and the left edge of the rectangle.
The number of inches between the top edge of the fabric and the top edge of the rectangle.
The width of the rectangle in inches.
The height of the rectangle in inches.

A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left
edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square
inches of fabric represented by # (and ignores the square inches of fabric represented by .) in the
diagram below:

...........
...........
...#####...
...#####...
...#####...
...#####...
...........
...........
...........

The problem is that many of the claims overlap, causing two or more claims to cover part of the
same areas. For example, consider the following claims:

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2

Visually, these claim the following areas:

........
...2222.
...2222.
.11XX22.
.11XX22.
.111133.
.111133.
........

The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the
others, does not overlap either of them.)

If the Elves all proceed with their own plans, none of them will have enough fabric. How many
square inches of fabric are within two or more claims?

--- Part Two ---

Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of
fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be able
to make Santa's suit after all!

For example, in the claims above, only claim 3 is intact after all claims are made.

What is the ID of the only claim that doesn't overlap?

*/

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

#[derive(PartialEq, Debug)]
struct Claim {
  id: String,
  left_inches: usize,
  top_inches: usize,
  width: usize,
  height: usize,
}

fn build_claim_regex() -> Regex {
  Regex::new(r"#(.+?) @ (.+?),(.+?): (.+?)x(.+?)$").unwrap()
}

fn parse_str_into_claim(full_str: &str, reg: &mut Regex) -> Result<Claim, String> {
  let caps = reg.captures(full_str).unwrap();

  let claim = Claim {
    id: caps.get(1).unwrap().as_str().to_string(),
    left_inches: caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
    top_inches: caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
    width: caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
    height: caps.get(5).unwrap().as_str().parse::<usize>().unwrap(),
  };

  Ok(claim)
}

fn get_claims() -> Vec<Claim> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let mut reg = build_claim_regex();

  let claims: Vec<Claim> = contents
    .lines()
    .map(|x| parse_str_into_claim(&x, &mut reg).unwrap())
    .collect();

  claims
}

type CoveredSquares = HashMap<(usize, usize), usize>;

fn get_covered_squares(claims: &[Claim]) -> CoveredSquares {
  let mut max_x = 0;
  let mut max_y = 0;

  for claim in claims {
    let new_x = claim.left_inches + claim.width;
    let new_y = claim.top_inches + claim.height;

    if new_x > max_x {
      max_x = new_x;
    }

    if new_y > max_y {
      max_y = new_y;
    }
  }

  let mut covered_squares = HashMap::with_capacity(max_x * max_y);

  for claim in claims {
    for x in claim.left_inches..(claim.left_inches + claim.width) {
      for y in claim.top_inches..(claim.top_inches + claim.height) {
        let counter = covered_squares.entry((x, y)).or_insert(0);
        *counter += 1;
      }
    }
  }

  covered_squares
}

fn get_overlapping_claims_squares_count(
  claims: &[Claim],
  covered_squares: Option<&mut CoveredSquares>,
) -> usize {
  let mut r: CoveredSquares = match covered_squares {
    Some(_) => HashMap::new(),
    None => get_covered_squares(&claims),
  };
  let covered_squares = covered_squares.unwrap_or(&mut r);
  let mut num = 0;

  for &mut value in covered_squares.values_mut() {
    if value > 1 {
      num += 1;
    }
  }

  num
}

fn get_claims_without_overlap(
  claims: &[Claim],
  covered_squares: Option<&mut CoveredSquares>,
) -> Vec<String> {
  let mut r: CoveredSquares = match covered_squares {
    Some(_) => HashMap::new(),
    None => get_covered_squares(&claims),
  };
  let covered_squares = covered_squares.unwrap_or(&mut r);
  let mut found_claims = vec![];

  for claim in claims {
    let mut is_valid = true;

    for x in claim.left_inches..(claim.left_inches + claim.width) {
      for y in claim.top_inches..(claim.top_inches + claim.height) {
        let point = (x, y);
        let counter = covered_squares.get(&point).unwrap();

        if *counter != 1 {
          is_valid = false;
          break;
        }
      }

      if is_valid != true {
        break;
      }
    }

    if is_valid {
      found_claims.push(claim.id.clone());
      break;
    }
  }

  found_claims
}

fn main() {
  let claims = get_claims();

  let mut covered_squares = get_covered_squares(&claims);

  let overlapping_squares =
    get_overlapping_claims_squares_count(&claims, Some(&mut covered_squares));
  let claims_without_overlap = get_claims_without_overlap(&claims, Some(&mut covered_squares));

  if claims_without_overlap.iter().count() != 1 {
    panic!("Unexpected num of claims withtou overlap");
  }

  println!("Results:");
  println!("- (1) overlapping squares: {}", overlapping_squares);
  println!(
    "- (2) claim id without overlap: {:?}",
    claims_without_overlap[0]
  );
}

#[cfg(test)]
mod tests {
  use super::*;

  fn parse_str_into_claim_with_regex(s: &str) -> Result<Claim, String> {
    parse_str_into_claim(&s.to_string(), &mut build_claim_regex())
  }

  #[test]
  fn test_parse_str_into_claim() {
    let result = parse_str_into_claim_with_regex("#1 @ 2,3: 4x5").unwrap();
    assert_eq!(
      Claim {
        id: "1".to_string(),
        left_inches: 2,
        top_inches: 3,
        height: 5,
        width: 4
      },
      result
    );
  }

  #[test]
  fn test_get_overlapping_claims_squares_count_1() {
    let claims = vec![
      Claim {
        id: "1".to_string(),
        left_inches: 0,
        top_inches: 0,
        height: 1,
        width: 1,
      },
      Claim {
        id: "1".to_string(),
        left_inches: 0,
        top_inches: 0,
        height: 2,
        width: 2,
      },
    ];

    assert_eq!(get_overlapping_claims_squares_count(&claims, None), 1);
  }

  #[test]
  fn test_get_overlapping_claims_squares_count_2() {
    let claims = vec![
      Claim {
        id: "1".to_string(),
        left_inches: 0,
        top_inches: 0,
        height: 2,
        width: 3,
      },
      Claim {
        id: "2".to_string(),
        left_inches: 0,
        top_inches: 0,
        height: 3,
        width: 1,
      },
    ];

    assert_eq!(get_overlapping_claims_squares_count(&claims, None), 2);
  }

  #[test]
  fn test_get_overlapping_claims_squares_count_3() {
    let claims = vec![
      parse_str_into_claim_with_regex("#1 @ 100,50: 1x100").unwrap(),
      parse_str_into_claim_with_regex("#2 @ 100,50: 1x100").unwrap(),
    ];

    assert_eq!(get_overlapping_claims_squares_count(&claims, None), 100);
  }

  #[test]
  fn test_get_overlapping_claims_squares_count_example() {
    let claims = vec![
      parse_str_into_claim_with_regex("#1 @ 1,3: 4x4").unwrap(),
      parse_str_into_claim_with_regex("#2 @ 3,1: 4x4").unwrap(),
      parse_str_into_claim_with_regex("#3 @ 5,5: 2x2").unwrap(),
    ];

    assert_eq!(get_overlapping_claims_squares_count(&claims, None), 4);
  }
}
