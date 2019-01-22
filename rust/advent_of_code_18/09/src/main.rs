/*

--- Day 9: Marble Mania ---

You talk to the Elves while you wait for your navigation system to initialize. To pass the time,
they introduce you to their favorite marble game.

The Elves play this game by taking turns arranging the marbles in a circle according to very
particular rules. The marbles are numbered starting with 0 and increasing by 1 until every marble
has a number.

First, the marble numbered 0 is placed in the circle. At this point, while it contains only a
single marble, it is still a circle: the marble is both clockwise from itself and counter-clockwise
from itself. This marble is designated the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining marble into the circle between
the marbles that are 1 and 2 marbles clockwise of the current marble. (When the circle is large
enough, this means that there is one marble between the marble that was just placed and the current
marble.) The marble that was just placed then becomes the current marble.

However, if the marble that is about to be placed has a number which is a multiple of 23, something
entirely different happens. First, the current player keeps the marble they would have placed,
adding it to their score. In addition, the marble 7 marbles counter-clockwise from the current
marble is removed from the circle and also added to the current player's score. The marble located
immediately clockwise of the marble that was removed becomes the new current marble.

For example, suppose there are 9 players. After the marble with value 0 is placed in the middle,
each player (shown in square brackets) takes a turn. The result of each of those turns would
produce circles of marbles like this, where clockwise is to the right and the resulting current
marble is in parentheses:

[-] (0)
[1]  0 (1)
[2]  0 (2) 1
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3
[5]  0  4  2 (5) 1  3
[6]  0  4  2  5  1 (6) 3
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7
[9]  0  8  4 (9) 2  5  1  6  3  7
[1]  0  8  4  9  2(10) 5  1  6  3  7
[2]  0  8  4  9  2 10  5(11) 1  6  3  7
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last marble is used up. Assuming the
example above ends after the marble numbered 25, the winning score is 23+9=32 (because player 5
kept marble 23 and removed marble 9, while no other player got any points in this very short
example game).

Here are a few more examples:

10 players; last marble is worth 1618 points: high score is 8317
13 players; last marble is worth 7999 points: high score is 146373
17 players; last marble is worth 1104 points: high score is 2764
21 players; last marble is worth 6111 points: high score is 54718
30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?

--- Part Two ---

Amused by the speed of your answer, the Elves are curious:

What would the new winning Elf's score be if the number of the last marble were 100 times larger?

*/

use std::collections::VecDeque;

type PlayersNum = usize;
type Score = usize;
type LastMarbleScore = Score;
type GameDescription = (PlayersNum, LastMarbleScore);

const INPUT_TXT: GameDescription = (478, 71240);

fn get_idx_to_substract(idx: usize, total: usize, to_remove: usize) -> usize {
  let mut result = idx as i32 - to_remove as i32;

  if result < 0 {
    result += total as i32;
  }

  result as usize
}

// First personal implementation (not using a deque), time for 1st problem is ok (<5 seconds) but
// not for 2nd (> 1 minute)
fn get_high_score_for_game_description(game_description: GameDescription) -> Score {
  let (players_num, last_marble_score) = game_description;
  let mut players_scores: Vec<Score> = vec![];
  let mut marbles = vec![0];

  let mut current_marble_idx = 0;
  let mut current_player_idx = players_num - 1;
  let mut current_marble_value = 0;
  let mut marbles_since_last_score = 0;

  for player_idx in 0..players_num {
    players_scores.insert(player_idx, 0);
  }

  loop {
    current_marble_value += 1;
    marbles_since_last_score += 1;
    current_player_idx = (current_player_idx + 1) % players_num;
    let total_placed_marbles = marbles.len();

    if marbles_since_last_score == 23 {
      marbles_since_last_score = 0;

      let other_marble_to_remove_idx =
        get_idx_to_substract(current_marble_idx, total_placed_marbles, 7);

      let removed_marble_value = marbles.remove(other_marble_to_remove_idx);

      current_marble_idx = if other_marble_to_remove_idx == total_placed_marbles - 1 {
        0
      } else {
        other_marble_to_remove_idx
      };

      let current_player_score = players_scores[current_player_idx];
      let last_marble_worth = current_marble_value + removed_marble_value;

      players_scores[current_player_idx] = current_player_score + last_marble_worth;
    } else {
      let next_marble_idx = match total_placed_marbles - current_marble_idx {
        1 => 1,
        2 => total_placed_marbles,
        _ => current_marble_idx + 2,
      };

      marbles.insert(next_marble_idx, current_marble_value);

      current_marble_idx = next_marble_idx;
    }

    if current_marble_value % 100_000 == 0 {
      println!("current_marble_value {}", current_marble_value);
    }

    if current_marble_value >= last_marble_score {
      break;
    }
  }

  *players_scores.iter().max().unwrap()
}

// clockwise: positions > 0, counter-clockwise: positions < 0
fn rotate_ring_start_index<T>(ring: &mut VecDeque<T>, positions: i32) {
  if positions > 0 {
    for _ in 0..positions {
      let old_first = ring.pop_front().unwrap();

      ring.push_back(old_first);
    }
  } else if positions < 0 {
    for _ in positions..0 {
      let old_last = ring.pop_back().unwrap();

      ring.push_front(old_last);
    }
  }
}

// Searched for an optimized version for problem 2, updated it afterwards:
// (note in original version the first for-loop was wrong)
// original: https://www.reddit.com/r/adventofcode/comments/a4i97s/2018_day_9_solutions/ebfp2zp
fn calculate_high_score(game_description: GameDescription) -> Score {
  let (players_num, marbles_num) = game_description;
  let mut players_score = vec![0; players_num];
  let mut ring = VecDeque::new();

  ring.push_front(0);

  for current_marble in 1..=marbles_num {
    if current_marble % 23 == 0 {
      rotate_ring_start_index(&mut ring, -7);

      let player_idx = current_marble % players_num;
      let removed_marble = ring.pop_front().unwrap();
      let new_extra_score = current_marble + removed_marble;

      players_score[player_idx] += new_extra_score;
    } else {
      rotate_ring_start_index(&mut ring, 2);

      ring.push_front(current_marble);
    }
  }

  *players_score.iter().max().unwrap()
}

fn main() {
  let first_result = get_high_score_for_game_description(INPUT_TXT);
  let second_result = calculate_high_score((INPUT_TXT.0, INPUT_TXT.1 * 100));

  println!("Results:");
  println!("- (1) first high score: {}", first_result);
  println!("- (2) second high score: {}", second_result);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_examples() -> Vec<(GameDescription, Score)> {
    vec![
      ((9, 25), 32),
      ((10, 1618), 8317),
      ((13, 7999), 146_373),
      ((17, 1104), 2764),
      ((21, 6111), 54718),
      ((30, 5807), 37305),
    ]
  }

  #[test]
  fn test_get_idx_to_substract() {
    assert_eq!(get_idx_to_substract(0, 1, 0), 0);
    assert_eq!(get_idx_to_substract(9, 10, 7), 2);
    assert_eq!(get_idx_to_substract(0, 2, 1), 1);
  }

  #[test]
  fn test_get_high_score_for_game_description() {
    for (game_description, high_score) in get_examples() {
      let result = get_high_score_for_game_description(game_description);

      assert_eq!(result, high_score);
    }
  }

  #[test]
  fn test_calculate_high_score() {
    for (game_description, high_score) in get_examples() {
      let result = calculate_high_score(game_description);

      assert_eq!(result, high_score);
    }
  }
}
