/*

https://adventofcode.com/2018/day/2

--- Day 2: Inventory Management System --- You stop falling through time, catch your breath, and
check the screen on the device. "Destination reached. Current Year: 1518. Current Location: North
Pole Utility Closet 83N10." You made it! Now, to find those anomalies.

Outside the utility closet, you hear footsteps and a voice. "...I'm not sure either. But now that
so many people have chimneys, maybe he could sneak in that way?" Another voice responds, "Actually,
we've been working on a new kind of suit that would let him fit through tight spaces like that.
But, I heard that a few days ago, they lost the prototype fabric, the design plans, everything!
Nobody on the team can even seem to remember important details of the project!"

"Wouldn't they have had enough fabric to fill several boxes in the warehouse? They'd be stored
together, so the box IDs should be similar. Too bad it would take forever to search the warehouse
for two similar box IDs..." They walk too far away to hear any more.

Late at night, you sneak to the warehouse - who knows what kinds of paradoxes you could cause if
you were discovered - and use your fancy wrist device to quickly scan every box and produce a list
of the likely candidates (your puzzle input).

To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number
that have an ID containing exactly two of any letter and then separately counting those with
exactly three of any letter. You can multiply those two counts together to get a rudimentary
checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

abcdef contains no letters that appear exactly two or three times.

bababc contains two a and three b, so it counts for both.

abbcde contains two b, but no letter appears exactly three times.

abcccd contains three c, but no letter appears exactly two times.

aabcdd contains two a and two d, but it only counts once.  abcdee contains two e.

ababab contains three a and three b, but it only counts once.

Of these box IDs, four of them contain a letter which appears exactly twice, and
three of them contain a letter which appears exactly three times. Multiplying these together
produces a checksum of 4 * 3 = 12.

What is the checksum for your list of box IDs?

--- Part Two ---
Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype
fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings.
For example, given the following box IDs:

abcde

fghij

klmno

pqrst

fguij

axcye

wvxyz

The IDs abcde and axcye are close, but they differ by two characters (the second and fourth).
However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must
be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by
                                                          removing the differing character from
                                                          either ID, producing fgij.)

*/

use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

fn get_ids() -> Vec<String> {
  let mut file = File::open("src/ids_list.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let ids: Vec<String> = contents.lines().map(|x| x.to_string()).collect();

  ids
}

fn get_checksum_of_ids(ids: &Vec<String>) -> usize {
  let mut strs_with_2_letters = 0;
  let mut strs_with_3_letters = 0;

  for id in ids {
    let mut occurrences = HashMap::new();
    let mut is_2_letters_match_found = false;
    let mut is_3_letters_match_found = false;

    for c in id.chars() {
      let counter = occurrences.entry(c).or_insert(0);
      *counter += 1;
    }

    for key in occurrences.keys() {
      if !is_2_letters_match_found {
        let letter_occ = match occurrences.get(key) {
          Some(&val) => val,
          None => 0,
        };

        if letter_occ == 2 {
          strs_with_2_letters += 1;
          is_2_letters_match_found = true;
        }
      }

      if !is_3_letters_match_found {
        let letter_occ = match occurrences.get(key) {
          Some(&val) => val,
          None => 0,
        };

        if letter_occ == 3 {
          strs_with_3_letters += 1;
          is_3_letters_match_found = true;
        }
      }
    }
  }

  return strs_with_3_letters * strs_with_2_letters;
}

fn get_diff_chars_in_str(a: &String, b: &String) -> usize {
  let mut diff_chars = 0;

  for (idx, a_char) in a.chars().enumerate() {
    let mut b_chars = b.chars();

    if a_char != b_chars.nth(idx).unwrap() {
      diff_chars += 1;
    }
  }

  diff_chars
}

fn get_ids_with_one_letter_different(ids: &Vec<String>) -> Vec<String> {
  let mut new_ids = vec![];

  for (id_idx, id) in ids.iter().enumerate() {
    for another_id in &ids[id_idx + 1..] {
      if get_diff_chars_in_str(&id, &another_id) == 1 {
        new_ids.push(id.clone());
        new_ids.push(another_id.clone());
      }
    }
  }

  new_ids
}

fn get_common_letters_of_str(a: &String, b: &String) -> String {
  let mut new_str = String::new();

  for (idx, a_char) in a.chars().enumerate() {
    let mut b_chars = b.chars();

    if a_char == b_chars.nth(idx).unwrap() {
      new_str.push(a_char);
    }
  }

  new_str
}

fn main() {
  let ids = get_ids();
  let checksum = get_checksum_of_ids(&ids);
  let matching_ids = get_ids_with_one_letter_different(&ids);

  if matching_ids.iter().count() != 2 {
    panic!("Unexpected ids number");
  }

  let common_letters =
    get_common_letters_of_str(matching_ids.get(0).unwrap(), matching_ids.get(1).unwrap());

  println!("Results:");
  println!("- (1) checksum: {}", checksum);
  println!("- (2) same letters: {}", common_letters);
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_checksum_of_ids() {
    let ids = vec![
      "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab",
    ].iter()
      .map(|x| x.to_string())
      .collect();
    let checksum = get_checksum_of_ids(&ids);

    assert_eq!(12, checksum);
  }

  #[test]
  fn test_get_ids_with_one_letter_different() {
    let ids = vec![
      "abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz",
    ].iter()
      .map(|x| x.to_string())
      .collect();
    let new_ids = get_ids_with_one_letter_different(&ids);

    assert_eq!(2, new_ids.iter().count());
  }

  #[test]
  fn test_get_common_letters_of_str() {
    let result = get_common_letters_of_str(&"foo".to_string(), &"fio".to_string());
    assert_eq!("fo", result);
  }
}
