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

--- Part Two ---

Time to improve the polymer.

One of the unit types is causing problems; it's preventing the polymer from collapsing as much as
it should. Your goal is to figure out which unit type is causing the most problems, remove all
instances of it (regardless of polarity), fully react the remaining polymer, and measure its
length.

For example, again using the polymer dabAcCaCBAcCcaDA from above:

Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer produces dbCBcD, which has length 6.
Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this polymer produces daCAcaDA, which has length 8.
Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer produces daDA, which has length 4.
Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this polymer produces abCBAc, which has length 6.

In this example, removing all C/c units was best, producing the answer 4.

What is the length of the shortest polymer you can produce by removing all units of exactly one
type and fully reacting the result?

*/

use std::fs::File;
use std::io::prelude::*;

type Polymer = Vec<char>;

fn get_input_polymer() -> Polymer {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  contents.chars().collect()
}

// optimized using: https://sts10.github.io/2018/12/07/optimizing-rust-advent-of-code-day-5.html
// for exercise 1
fn react(mut polymer: Polymer) -> Polymer {
  let mut p_vec_len = polymer.len();
  let mut previous_c: char;
  let mut index = 1;

  while index < p_vec_len {
    previous_c = polymer[index - 1];

    if do_these_two_chars_cancel(polymer[index], previous_c) {
      polymer.drain((index - 1)..=index);
      p_vec_len -= 2;

      if index > 1 {
        index -= 1
      }
    } else {
      index += 1;
    }
  }

  polymer
}

fn get_shortest_polymer_length_by_removing_one_type(polymer: Polymer) -> usize {
  let mut lowest = polymer.len();

  for letter_num in b'a'..=b'z' {
    let letter = letter_num as char;
    let mut new_polymer = polymer.clone();
    let letter_uppercase = letter.to_uppercase().to_string();

    new_polymer.retain(|&c| {
      c.to_uppercase().to_string() != letter_uppercase
    });

    new_polymer = react(new_polymer);

    let len = new_polymer.len();


    if len < lowest {
      lowest = len;
    }
  }

  lowest
}

fn do_these_two_chars_cancel(a: char, b: char) -> bool {
  a.eq_ignore_ascii_case(&b) && a.is_uppercase() == b.is_lowercase()
}

fn main() {
  let start_polymer = get_input_polymer();
  let result_a = react(start_polymer.clone());
  let result_b = get_shortest_polymer_length_by_removing_one_type(start_polymer.clone());

  println!("Results:");
  println!("- (1) final polymer length: {}", result_a.len());
  println!("- (2) final polymer length: {}", result_b);
}

#[cfg(test)]
mod tests {
  use super::*;

  use std::iter::FromIterator;

  #[test]
  fn test_react() {
    fn test_this(s1: &str, s2: &str) {
      let result = react(s1.to_string().chars().collect());

      assert_eq!(String::from_iter(result), s2);
    }

    test_this("dabAcCaCBAcCcaDA", "dabCBAcaDA");
    test_this("abcC", "ab");
  }

  #[test]
  fn test_get_shortest_polymer_length_by_removing_one_type() {
    let polymer = "dabAcCaCBAcCcaDA".chars().collect();
    let result = get_shortest_polymer_length_by_removing_one_type(polymer);

    assert_eq!(result, 4);
  }
}
