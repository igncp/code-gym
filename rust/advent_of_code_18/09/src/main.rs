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

*/

type PlayersNum = usize;
type Score = usize;
type LastMarbleScore = Score;
type GameDescription = (PlayersNum, LastMarbleScore);

const INPUT_TXT: GameDescription = (478, 71240);

fn get_idx_to_substract(idx: usize, total: usize, to_remove: usize) -> usize {
  let mut result = idx as i32 - to_remove as i32;

  if result < 0 {
    result = (total as i32) + result;
  }

  result as usize
}

fn get_high_score_for_game_description(
  game_description: GameDescription,
) -> (Score, Vec<(usize, Vec<usize>)>) {
  let (players_num, last_marble_score) = game_description;
  let mut players_scores: Vec<Score> = vec![];
  let mut marbles = vec![0];
  let mut first_marbles_combinations: Vec<(usize, Vec<usize>)> = vec![];

  let mut current_marble_idx = 0;
  let mut current_player_idx = players_num - 1;
  let mut current_marble_value = 0;
  let mut marbles_since_last_score = 0;

  let mut has_reached_score = false;

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

      current_marble_idx = match other_marble_to_remove_idx == total_placed_marbles - 1 {
        true => 0,
        _ => other_marble_to_remove_idx,
      };

      let current_player_score = *players_scores.get(current_player_idx).unwrap();
      let last_marble_worth = current_marble_value + removed_marble_value;

      players_scores[current_player_idx] = current_player_score + last_marble_worth;

      if last_marble_worth >= last_marble_score {
        has_reached_score = true;
      };
    } else {
      let next_marble_idx = match total_placed_marbles - current_marble_idx {
        1 => 1,
        2 => total_placed_marbles,
        _ => current_marble_idx + 2,
      };

      marbles.insert(next_marble_idx, current_marble_value);

      current_marble_idx = next_marble_idx;
    }

    if current_marble_value < 35 {
      first_marbles_combinations.push((current_player_idx + 1, marbles.clone()));
    } else if has_reached_score {
      break;
    }
  }

  (
    *players_scores.iter().max().unwrap(),
    first_marbles_combinations,
  )
}

fn main() {
  println!("Results:");
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_examples() -> Vec<(GameDescription, Score)> {
    vec![
      ((9, 32), 32),
      ((10, 1618), 8317),
      // ((13, 7999), 146373),
      // ((17, 1104), 2764),
      // ((21, 6111), 54718),
      // ((30, 5807), 37305),
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
      let (result, _) = get_high_score_for_game_description(game_description);

      assert_eq!(result, high_score);
    }
  }

  #[test]
  fn test_first_combinations() {
    let (_, combinations) = get_high_score_for_game_description((9, 32));
    let expected_result: Vec<(usize, Vec<usize>)> = vec![
      (1, vec![0, 1]),
      (2, vec![0, 2, 1]),
      (3, vec![0, 2, 1, 3]),
      (4, vec![0, 4, 2, 1, 3]),
      (5, vec![0, 4, 2, 5, 1, 3]),
      (6, vec![0, 4, 2, 5, 1, 6, 3]),
      (7, vec![0, 4, 2, 5, 1, 6, 3, 7]),
      (8, vec![0, 8, 4, 2, 5, 1, 6, 3, 7]),
      (9, vec![0, 8, 4, 9, 2, 5, 1, 6, 3, 7]),
      (1, vec![0, 8, 4, 9, 2, 10, 5, 1, 6, 3, 7]),
      (2, vec![0, 8, 4, 9, 2, 10, 5, 11, 1, 6, 3, 7]),
      (3, vec![0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 3, 7]),
      (4, vec![0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 7]),
      (5, vec![0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7]),
      (
        6,
        vec![0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15],
      ),
      (
        7,
        vec![0, 16, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15],
      ),
      (
        8,
        vec![0, 16, 8, 17, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15],
      ),
      (
        9,
        vec![
          0, 16, 8, 17, 4, 18, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        1,
        vec![
          0, 16, 8, 17, 4, 18, 9, 19, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        2,
        vec![
          0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        3,
        vec![
          0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        4,
        vec![
          0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        5,
        vec![
          0, 16, 8, 17, 4, 18, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        6,
        vec![
          0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
      (
        7,
        vec![
          0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 25, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15,
        ],
      ),
    ];

    for (idx, comb) in expected_result.iter().enumerate() {
      let result_comb = combinations.get(idx).unwrap();
      assert_eq!(result_comb, comb);
    }
  }
}
