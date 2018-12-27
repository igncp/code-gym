/*

--- Day 5: Alchemical Reduction ---

You've managed to sneak in to the prototype suit manufacturing lab. The Elves are making decent
progress, but are still struggling with the suit's size reduction capabilities.

While the very latest in 1518 alchemical technology might have solved their problem eventually, you
can do better. You scan the chemical composition of the suit's material and discover that it is
formed by extremely long polymers (one of which is available as your puzzle input).

The polymer is formed by smaller units which, when triggered, react with each other such that two
adjacent units of the same type and opposite polarity are destroyed. Units' types are represented
by letters; units' polarity is represented by capitalization. For instance, r and R are units with
the same type but opposite polarity, whereas r and s are entirely different types and do not react.

For example:

In aA, a and A react, leaving nothing behind.
In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
In abAB, no two adjacent units are of the same type, and so nothing happens.
In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.

Now, consider a larger example, dabAcCaCBAcCcaDA:

dabAcCaCBAcCcaDA  The first 'cC' is removed.
dabAaCBAcCcaDA    This creates 'Aa', which is removed.
dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
dabCBAcaDA        No further actions can be taken.

After all possible reactions, the resulting polymer contains 10 units.

How many units remain after fully reacting the polymer you scanned? (Note: in this puzzle and
others, the input is large; if you copy/paste your input, make sure you get the whole thing.)

*/

use std::fs::File;
use std::io::prelude::*;

type Polymer = String;

fn get_input_polymer() -> String {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  contents
}

fn get_are_char_opposites(a: char, b: char) -> bool {
  if a == b {
    return false;
  }

  if a.to_uppercase().nth(0).unwrap() == b {
    return true;
  }

  if a.to_lowercase().nth(0).unwrap() == b {
    return true;
  }

  false
}

// TODO: Optmize, this gives the correct solution but takes around 5 mins
fn parse_polymer(polymer: Polymer) -> Polymer {
  let mut new_polymer = polymer.clone();

  loop {
    let mut has_finished = false;
    let temp_polymer = new_polymer.clone();
    let all_chars = temp_polymer.chars();
    let length = all_chars.clone().count();

    for (idx, c) in all_chars.clone().enumerate() {
      if idx == 0 {
        continue;
      }

      let prev_char = all_chars.clone().nth(idx - 1).unwrap();

      if get_are_char_opposites(prev_char, c) {
        new_polymer = [
          new_polymer[0..idx - 1].to_string(),
          new_polymer[idx + 1..].to_string(),
        ]
        .join("");
        break;
      }

      if idx == length - 1 {
        has_finished = true;
      }
    }

    if has_finished {
      break;
    }
  }

  new_polymer
}

fn main() {
  let start_polymer = get_input_polymer();
  let result = parse_polymer(start_polymer);

  println!("Results:");
  println!("- (1) final polymer count: {}", result.chars().count());
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_polymer() {
    fn test_this(s1: &str, s2: &str) {
      let result = parse_polymer(s1.to_string());

      assert_eq!(result, s2);
    }

    test_this("dabAcCaCBAcCcaDA", "dabCBAcaDA");
    test_this("abcC", "ab");
  }

  #[test]
  fn test_get_are_char_opposites() {
    assert_eq!(get_are_char_opposites('a', 'A'), true);
    assert_eq!(get_are_char_opposites('A', 'a'), true);
    assert_eq!(get_are_char_opposites('a', 'a'), false);
    assert_eq!(get_are_char_opposites('A', 'A'), false);
    assert_eq!(get_are_char_opposites('b', 'A'), false);
  }
}
