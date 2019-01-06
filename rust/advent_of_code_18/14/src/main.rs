/*

--- Day 14: Chocolate Charts ---

You finally have a chance to look at all of the produce moving around. Chocolate, cinnamon, mint,
chili peppers, nutmeg, vanilla... the Elves must be growing these plants to make hot chocolate! As
you realize this, you hear a conversation in the distance. When you go to investigate, you discover
two Elves in what appears to be a makeshift underground kitchen/laboratory.

The Elves are trying to come up with the ultimate hot chocolate recipe; they're even maintaining a
scoreboard which tracks the quality score (0-9) of each recipe.

Only two recipes are on the board: the first recipe got a score of 3, the second, 7. Each of the
two Elves has a current recipe: the first Elf starts with the first recipe, and the second Elf
starts with the second recipe.

To create new recipes, the two Elves combine their current recipes. This creates new recipes from
the digits of the sum of the current recipes' scores. With the current recipes' scores of 3 and 7,
their sum is 10, and so two new recipes would be created: the first with score 1 and the second
with score 0. If the current recipes' scores were 2 and 3, the sum, 5, would only create one recipe
(with a score of 5) with its single digit.

The new recipes are added to the end of the scoreboard in the order they are created. So, after the
first round, the scoreboard is 3, 7, 1, 0.

After all new recipes are added to the scoreboard, each Elf picks a new current recipe. To do this,
the Elf steps forward through the scoreboard a number of recipes equal to 1 plus the score of their
current recipe. So, after the first round, the first Elf moves forward 1 + 3 = 4 times, while the
second Elf moves forward 1 + 7 = 8 times. If they run out of recipes, they loop back around to the
beginning. After the first round, both Elves happen to loop around until they land on the same
recipe that they had in the beginning; in general, they will move to different recipes.

Drawing the first Elf as parentheses and the second Elf as square brackets, they continue this
process:

(3)[7]
(3)[7] 1  0
 3  7  1 [0](1) 0
 3  7  1  0 [1] 0 (1)
(3) 7  1  0  1  0 [1] 2
 3  7  1  0 (1) 0  1  2 [4]
 3  7  1 [0] 1  0 (1) 2  4  5
 3  7  1  0 [1] 0  1  2 (4) 5  1
 3 (7) 1  0  1  0 [1] 2  4  5  1  5
 3  7  1  0  1  0  1  2 [4](5) 1  5  8
 3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
 3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6
 3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7
 3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7
 3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9
 3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2

The Elves think their skill will improve after making a few recipes (your puzzle input). However,
that could take ages; you can speed this up considerably by identifying the scores of the ten
recipes after that. For example:

If the Elves think their skill will improve after making 9 recipes, the scores of the ten recipes
after the first nine on the scoreboard would be 5158916779 (highlighted in the last line of the
diagram).

After 5 recipes, the scores of the next ten would be 0124515891.

After 18 recipes, the scores of the next ten would be 9251071085.

After 2018 recipes, the scores of the next ten would be 5941429882.

What are the scores of the ten recipes immediately after the number of recipes in your puzzle
input?

--- Part Two ---

As it turns out, you got the Elves' plan backwards. They actually want to know how many recipes
appear on the scoreboard to the left of the first recipes whose scores are the digits from your
puzzle input.

51589 first appears after 9 recipes.
01245 first appears after 5 recipes.
92510 first appears after 18 recipes.
59414 first appears after 2018 recipes.

How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?

*/

const INPUT_RECIPES_NUM: usize = 846021;

#[derive(Debug, Clone)]
struct RecipesState {
  recipes: Vec<usize>,
  elf_1_pos: usize,
  elf_2_pos: usize,
}

fn get_initial_configuration() -> RecipesState {
  RecipesState {
    recipes: vec![3, 7],
    elf_1_pos: 0,
    elf_2_pos: 1,
  }
}

fn get_next_elf_idx(current_idx: usize, current_value: usize, recipes_len: usize) -> usize {
  let moves = current_value + 1;
  let mut new_idx = current_idx + moves;

  while new_idx > recipes_len - 1 {
    new_idx -= recipes_len;
  }

  new_idx
}

fn get_score_after_n_recipes(mut recipes_state: &mut RecipesState, n_steps: usize) -> String {
  let mut recipes_len = 2;

  while recipes_len < n_steps + 10 {
    recipes_len = update_recipes_state(&mut recipes_state, recipes_len);
  }

  let slice = recipes_state.recipes.get(n_steps..n_steps + 10).unwrap();
  let chars: Vec<String> = slice.iter().map(|x| x.to_string()).collect();

  chars.join("")
}

fn update_recipes_state(recipes_state: &mut RecipesState, orig_recipes_len: usize) -> usize {
  let mut recipes_len = orig_recipes_len;
  let elf_1_value = recipes_state.recipes[recipes_state.elf_1_pos];
  let elf_2_value = recipes_state.recipes[recipes_state.elf_2_pos];
  let sum = elf_1_value + elf_2_value;

  if sum > 9 {
    let digit_2 = sum % 10;
    let digit_1 = (sum - digit_2) / 10;

    recipes_state.recipes.push(digit_1);
    recipes_state.recipes.push(digit_2);
    recipes_len += 2;
  } else {
    recipes_state.recipes.push(sum);
    recipes_len += 1;
  }

  recipes_state.elf_1_pos = get_next_elf_idx(recipes_state.elf_1_pos, elf_1_value, recipes_len);
  recipes_state.elf_2_pos = get_next_elf_idx(recipes_state.elf_2_pos, elf_2_value, recipes_len);

  recipes_len
}

fn get_n_recipes_when_score(mut recipes_state: &mut RecipesState, score: String) -> usize {
  let mut recipes_len = 2;
  let mut last_tracked_idx = 0;
  let digits: Vec<usize> = score
    .chars()
    .map(|x| x.to_string().parse::<usize>().unwrap())
    .collect();
  let digits_num = digits.clone().iter().count();

  loop {
    recipes_len = update_recipes_state(&mut recipes_state, recipes_len);
    let mut rel_idx = 0;

    while last_tracked_idx + digits_num < recipes_len {
      let idx = rel_idx + last_tracked_idx;
      let value = recipes_state.recipes[idx];

      if value != digits[rel_idx] {
        last_tracked_idx += 1;
        rel_idx = 0;
      } else {
        rel_idx += 1;

        if rel_idx == digits_num {
          break;
        }
      }
    }

    if rel_idx == digits_num {
      break;
    }
  }

  last_tracked_idx
}

fn main() {
  let initial_configuration = get_initial_configuration();
  let first_exercise_result =
    get_score_after_n_recipes(&mut initial_configuration.clone(), INPUT_RECIPES_NUM);
  let second_exercise_result = get_n_recipes_when_score(
    &mut initial_configuration.clone(),
    INPUT_RECIPES_NUM.to_string(),
  );

  println!("Results:");
  println!("- (1) score result: {}", first_exercise_result);
  println!("- (2) recipes result: {}", second_exercise_result);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_results() -> Vec<(usize, String)> {
    vec![
      (5, "0124515891".to_string()),
      (9, "5158916779".to_string()),
      (18, "9251071085".to_string()),
      (2018, "5941429882".to_string()),
    ]
  }

  #[test]
  fn test_get_score_after_n_recipes() {
    let results = get_example_results();

    for result in results {
      let mut initial_configuration = get_initial_configuration();
      let score = get_score_after_n_recipes(&mut initial_configuration, result.0);

      assert_eq!(score, result.1);
    }
  }

  #[test]
  fn test_get_n_recipes_when_score() {
    let results = get_example_results();

    for result in results {
      let mut initial_configuration = get_initial_configuration();
      let new_string = result.1.to_string();
      let n_recipes = get_n_recipes_when_score(&mut initial_configuration, new_string);

      assert_eq!(n_recipes, result.0);
    }
  }
}
