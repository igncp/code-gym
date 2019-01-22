/*

--- Day 12: Subterranean Sustainability ---

The year 518 is significantly more underground than your history books implied. Either that, or
you've arrived in a vast cavern network under the North Pole.

After exploring a little, you discover a long tunnel that contains a row of small pots as far as
you can see to your left and right. A few of them contain plants - someone is trying to grow things
in these geothermally-heated caves.

The pots are numbered, with 0 in front of you. To the left, the pots are numbered -1, -2, -3, and
so on; to the right, 1, 2, 3.... Your puzzle input contains a list of pots from 0 to the right and
whether they do (#) or do not (.) currently contain a plant, the initial state. (No other pots
currently contain plants.) For example, an initial state of #..##.... indicates that pots 0, 3, and
4 currently contain plants.

Your puzzle input also contains some notes you find on a nearby table: someone has been trying to
figure out how these plants spread to nearby pots. Based on the notes, for each generation of
plants, a given pot has or does not have a plant based on whether that pot (and the two pots on
either side of it) had a plant in the last generation. These are written as LLCRR => N, where L are
pots to the left, C is the current pot being considered, R are the pots to the right, and N is
whether the current pot will have a plant in the next generation. For example:

A note like ..#.. => . means that a pot that contains a plant but with no plants within two pots of
it will not have a plant in it during the next generation.

A note like ##.## => . means that an empty pot with two plants on each side of it will remain empty
in the next generation.

A note like .##.# => # means that a pot has a plant in a given generation if, in the previous
generation, there were plants in that pot, the one immediately to the left, and the one two pots to
the right, but not in the ones immediately to the right and two to the left.

It's not clear what these plants are for, but you're sure it's important, so you'd like to make
sure the current configuration of plants is sustainable by determining what will happen after 20
generations.

For example, given the following input:

initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #

For brevity, in this example, only the combinations which do produce a plant are listed. (Your
input includes all possible combinations.) Then, the next 20 generations will look like this:

                 1         2         3
       0         0         0         0
 0: ...#..#.#..##......###...###...........
 1: ...#...#....#.....#..#..#..#...........
 2: ...##..##...##....#..#..#..##..........
 3: ..#.#...#..#.#....#..#..#...#..........
 4: ...#.#..#...#.#...#..#..##..##.........
 5: ....#...##...#.#..#..#...#...#.........
 6: ....##.#.#....#...#..##..##..##........
 7: ...#..###.#...##..#...#...#...#........
 8: ...#....##.#.#.#..##..##..##..##.......
 9: ...##..#..#####....#...#...#...#.......
10: ..#.#..#...#.##....##..##..##..##......
11: ...#...##...#.#...#.#...#...#...#......
12: ...##.#.#....#.#...#.#..##..##..##.....
13: ..#..###.#....#.#...#....#...#...#.....
14: ..#....##.#....#.#..##...##..##..##....
15: ..##..#..#.#....#....#..#.#...#...#....
16: .#.#..#...#.#...##...#...#.#..##..##...
17: ..#...##...#.#.#.#...##...#....#...#...
18: ..##.#.#....#####.#.#.#...##...##..##..
19: .#..###.#..#.#.#######.#.#.#..#.#...#..
20: .#....##....#####...#######....#.#..##.

The generation is shown along the left, where 0 is the initial state. The pot numbers are shown
along the top, where 0 labels the center pot, negative-numbered pots extend to the left, and
positive pots extend toward the right. Remember, the initial state begins at pot 0, which is not
the leftmost pot used in this example.

After one generation, only seven plants remain. The one in pot 0 matched the rule looking for
..#.., the one in pot 4 matched the rule looking for .#.#., pot 9 matched .##.., and so on.

In this example, after 20 generations, the pots shown as # contain plants, the furthest left of
which is pot -2, and the furthest right of which is pot 34. Adding up all the numbers of
plant-containing pots after the 20th generation produces 325.

After 20 generations, what is the sum of the numbers of all pots which contain a plant?

--- Part Two ---

You realize that 20 generations aren't enough. After all, these plants will need to last another
1500 years to even reach your timeline, not to mention your future.

After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which
contain a plant?

*/

use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

type CombinationId = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum PotState {
  HasPlant,
  Empty,
}

#[derive(Debug, Copy, Clone)]
struct CombinationBranch {
  has_plant: Option<CombinationId>,
  empty: Option<CombinationId>,
}

#[derive(Debug, Clone)]
enum Combination {
  Branch(CombinationBranch),
  Node(PotState),
}

type CombinationsMap = HashMap<CombinationId, Combination>;
type PlantsState = Vec<bool>;

const OFFSET: usize = 1000;
const INITIAL_STATE: &str = "#.####...##..#....#####.##.......##.#..###.#####.###.##.###.###.#...#...##.#.##.#...#..#.##..##.#.##";

fn convert_state_str_to_vec(state: &str) -> PlantsState {
  let mut result: PlantsState = state.chars().map(|x| x == '#').collect();

  for _ in 0..OFFSET {
    result.insert(0, false);
    result.push(false);
  }

  result
}

fn get_id_for_combinations_map_item(
  combinations_map: &mut CombinationsMap,
  id: CombinationId,
  ch: char,
) -> Option<CombinationId> {
  if let Some(v) = combinations_map.get(&id) {
    if let Combination::Branch(w) = v {
      return if ch == '#' { w.has_plant } else { w.empty };
    }
  }

  None
}

fn convert_strs_to_combinations_map(combinations_strs: &mut Vec<String>) -> CombinationsMap {
  let mut combinations_map: CombinationsMap = HashMap::new();
  let mut current_combination_id = 1;

  combinations_map.insert(
    0,
    Combination::Branch(CombinationBranch {
      has_plant: None,
      empty: None,
    }),
  );

  for combination_str in combinations_strs {
    let mut prev_combination_id: Option<CombinationId> = None;

    fn update_prev_combination(
      combinations_map: &mut CombinationsMap,
      prev_id_raw: CombinationId,
      ch: char,
      combination_id: CombinationId,
    ) {
      let existing_combination = combinations_map.get(&prev_id_raw).unwrap();

      if let Combination::Branch(mut existing_combination_branch) = existing_combination {
        if ch == '#' {
          existing_combination_branch.has_plant = Some(combination_id);
        } else {
          existing_combination_branch.empty = Some(combination_id);
        }

        combinations_map.insert(
          prev_id_raw,
          Combination::Branch(existing_combination_branch),
        );
      }
    }

    for (idx, ch) in combination_str.chars().take(5).enumerate() {
      let mut combination_id = current_combination_id;
      let prev_id_raw = prev_combination_id.unwrap_or(0);

      combination_id = get_id_for_combinations_map_item(&mut combinations_map, prev_id_raw, ch)
        .unwrap_or(combination_id);

      // entry does not exist yet
      if current_combination_id == combination_id {
        if idx != 4 {
          combinations_map.insert(
            current_combination_id,
            Combination::Branch(CombinationBranch {
              has_plant: None,
              empty: None,
            }),
          );
        }

        update_prev_combination(&mut combinations_map, prev_id_raw, ch, combination_id);
      }

      prev_combination_id = Some(combination_id);
      current_combination_id += 1;
    }

    let ch = combination_str.chars().nth(9).unwrap();

    let node_content = if ch == '#' {
      PotState::HasPlant
    } else {
      PotState::Empty
    };

    combinations_map.insert(
      prev_combination_id.unwrap(),
      Combination::Node(node_content),
    );
  }

  combinations_map
}

fn get_result_for_combination_vec(
  combinations_map: &mut CombinationsMap,
  combination_vec: &mut PlantsState,
) -> Option<PotState> {
  let mut result: Option<PotState> = None;
  let mut prev_id: Option<CombinationId> = None;

  for item in combination_vec {
    let combination_id = prev_id.unwrap_or(0);

    if let Combination::Branch(combination_branch) = combinations_map.get(&combination_id).unwrap()
    {
      prev_id = if *item {
        combination_branch.has_plant
      } else {
        combination_branch.empty
      };

      if prev_id.is_none() {
        break;
      }
    }
  }

  if prev_id.is_some() {
    if let Combination::Node(pot_state) = combinations_map.get(&prev_id.unwrap()).unwrap() {
      result = Some(*pot_state);
    }
  }

  result
}

fn get_input_combinations() -> Vec<String> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let descriptions: Vec<String> = contents.lines().clone().map(|x| x.to_string()).collect();

  descriptions
}

fn get_new_state_after_one_generation(
  orig_state: &mut PlantsState,
  mut combinations_map: &mut CombinationsMap,
) -> PlantsState {
  let mut new_state: PlantsState = vec![];
  let len = orig_state.len();

  for idx in 0..len {
    if idx < 2 || idx >= len - 2 {
      new_state.push(orig_state[idx]);
      continue;
    }

    let mut combination_vec: PlantsState = vec![
      orig_state[idx - 2],
      orig_state[idx - 1],
      orig_state[idx],
      orig_state[idx + 1],
      orig_state[idx + 2],
    ];
    let new_state_item =
      match get_result_for_combination_vec(&mut combinations_map, &mut combination_vec)
        .unwrap_or(PotState::Empty)
      {
        PotState::HasPlant => true,
        PotState::Empty => false,
      };

    new_state.push(new_state_item);
  }

  new_state
}

fn get_new_state_after_n_generations(
  orig_state: &mut PlantsState,
  mut combinations_map: &mut CombinationsMap,
  n_generations: usize,
) -> PlantsState {
  let mut new_state: PlantsState = orig_state.clone();

  for _ in 0..n_generations {
    new_state = get_new_state_after_one_generation(&mut new_state, &mut combinations_map);
  }

  new_state
}

fn get_pots_with_plant_sum(plants_state: &mut PlantsState) -> i64 {
  let mut sum: i64 = 0;

  for (idx, state_item) in plants_state.iter().enumerate() {
    if *state_item {
      sum += idx as i64 - OFFSET as i64;
    }
  }

  sum
}

fn get_pots_with_plant_sum_using_pattern(
  orig_state: &mut PlantsState,
  mut combinations_map: &mut CombinationsMap,
  n_generations: usize,
) -> i64 {
  let mut sum: i64;
  let mut new_state: PlantsState = orig_state.clone();

  let mut last_idx: i64 = 100;

  let mut diff_a = 0;
  let mut diff_b = 0;
  let mut diff_c;

  // the number 100 is a random high-enough number found empirically
  new_state =
    get_new_state_after_n_generations(&mut new_state, &mut combinations_map, last_idx as usize);
  sum = get_pots_with_plant_sum(&mut new_state) as i64;

  for _ in 0..100 {
    diff_c = diff_b;
    diff_b = diff_a;

    let prev_sum = sum;
    new_state = get_new_state_after_n_generations(&mut new_state, &mut combinations_map, 1);
    sum = get_pots_with_plant_sum(&mut new_state) as i64;

    last_idx += 1;
    diff_a = sum - prev_sum;

    if diff_a != 0 && diff_a == diff_b && diff_b == diff_c {
      break;
    }
  }

  sum + diff_a * (n_generations as i64 - last_idx as i64)
}

fn main() {
  let mut input_combinations = get_input_combinations();
  let mut combinations_map = convert_strs_to_combinations_map(&mut input_combinations);
  let mut state_vector = convert_state_str_to_vec(INITIAL_STATE);

  let mut final_state_20 =
    get_new_state_after_n_generations(&mut state_vector, &mut combinations_map, 20);
  let sum_20 = get_pots_with_plant_sum(&mut final_state_20);

  let sum_5b =
    get_pots_with_plant_sum_using_pattern(&mut state_vector, &mut combinations_map, 50_000_000_000);

  println!("Results:");
  println!("- (1) sum of pots with plant for 20: {}", sum_20);
  println!("- (2) sum of pots with plant for 5b: {}", sum_5b);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_result_for_combination_str(
    combinations_map: &mut CombinationsMap,
    combination_str: &str,
  ) -> Option<PotState> {
    let mut result: Option<PotState> = None;
    let mut prev_id: Option<CombinationId> = None;

    for ch in combination_str.chars() {
      let combination_id = prev_id.unwrap_or(0);

      if let Combination::Branch(combination_branch) =
        combinations_map.get(&combination_id).unwrap()
      {
        let field = if ch == '#' {
          combination_branch.has_plant
        } else {
          combination_branch.empty
        };

        if field.is_some() {
          prev_id = field;
        } else {
          prev_id = None;
          break;
        }
      }
    }

    if prev_id.is_some() {
      if let Combination::Node(pot_state) = combinations_map.get(&prev_id.unwrap()).unwrap() {
        result = Some(*pot_state);
      }
    }

    result
  }

  fn get_example_combinations() -> Vec<String> {
    vec![
      "...## => #",
      "..#.. => #",
      ".#... => #",
      ".#.#. => #",
      ".#.## => #",
      ".##.. => #",
      ".#### => #",
      "#.#.# => #",
      "#.### => #",
      "##.#. => #",
      "##.## => #",
      "###.. => #",
      "###.# => #",
      "####. => #",
      "..... => .",
    ]
    .iter()
    .map(|x| x.to_string())
    .collect()
  }

  #[test]
  fn test_convert_state_str_to_vec() {
    let result = convert_state_str_to_vec("#..##");
    let mut expected = vec![true, false, false, true, true];

    for _ in 0..OFFSET {
      expected.insert(0, false);
    }

    for _ in 0..OFFSET {
      expected.push(false);
    }

    assert_eq!(result, expected)
  }

  #[test]
  fn test_convert_strs_to_combinations_map() {
    let mut combinations_strs = get_example_combinations();
    let mut combinations_map = convert_strs_to_combinations_map(&mut combinations_strs);

    assert_eq!(
      get_result_for_combination_str(&mut combinations_map, "...##"),
      Some(PotState::HasPlant)
    );
    assert_eq!(
      get_result_for_combination_str(&mut combinations_map, "#####"),
      None,
    );
    assert_eq!(
      get_result_for_combination_str(&mut combinations_map, "....."),
      Some(PotState::Empty),
    );
  }

  #[test]
  fn test_get_new_state_after_one_generation() {
    let mut combinations_strs = get_example_combinations();
    let mut combinations_map = convert_strs_to_combinations_map(&mut combinations_strs);
    let mut orig_state = convert_state_str_to_vec("#..#.#..##......###...###");
    let expected_final_state = convert_state_str_to_vec("#...#....#.....#..#..#..#");
    let new_state = get_new_state_after_one_generation(&mut orig_state, &mut combinations_map);

    assert_eq!(new_state, expected_final_state);
  }

  #[test]
  fn test_get_new_state_after_n_generations() {
    let mut combinations_strs = get_example_combinations();
    let mut combinations_map = convert_strs_to_combinations_map(&mut combinations_strs);
    let mut orig_state = convert_state_str_to_vec("...#..#.#..##......###...###...........");
    let expected_final_state = convert_state_str_to_vec(".#....##....#####...#######....#.#..##.");
    let new_state = get_new_state_after_n_generations(&mut orig_state, &mut combinations_map, 20);

    assert_eq!(new_state, expected_final_state);
  }

  #[test]
  fn test_get_pots_with_plant_sum() {
    let mut combinations_strs = get_example_combinations();
    let mut combinations_map = convert_strs_to_combinations_map(&mut combinations_strs);
    let mut orig_state = convert_state_str_to_vec("#..#.#..##......###...###");
    let mut new_state =
      get_new_state_after_n_generations(&mut orig_state, &mut combinations_map, 20);
    let sum = get_pots_with_plant_sum(&mut new_state);

    assert_eq!(sum, 325);
  }
}
