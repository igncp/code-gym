use std::collections::HashMap;
use std::iter::FromIterator;

use space::{Coord, Map, TerrainType};
use unit::{Unit, UnitType};

#[derive(Debug)]
pub struct Game {
  map: Map,
  has_finished: bool,
  rounds: usize,
  goblins: Vec<Unit>,
  elves: Vec<Unit>,
}

impl Game {
  pub fn new(text: String, elves_atack_power: Option<usize>) -> Self {
    let elves_atack_power = elves_atack_power.unwrap_or(3);
    let chars_vecs: Vec<Vec<char>> = text.lines().map(|x| x.chars().collect()).collect();
    let map = Map::new(&chars_vecs);
    let (goblins, mut elves) = Unit::get_units(&chars_vecs);

    for elve in elves.iter_mut() {
      elve.attack_power = elves_atack_power;
    }

    let mut game = Game {
      map: map,
      has_finished: false,
      rounds: 0,
      goblins: goblins,
      elves: elves,
    };

    game.set_obstacles();

    game
  }

  fn generate_str(&self) -> String {
    let mut lines: Vec<Vec<char>> = vec![];
    let mut map_hash: HashMap<Coord, char> = HashMap::new();

    for y in 0..self.map.dimensions.height {
      for x in 0..self.map.dimensions.width {
        let coord = Coord { x: x, y: y };
        let val = self.map.topology.get(&coord).unwrap();
        let ch = match val {
          TerrainType::Empty => '.',
          TerrainType::Wall => '#',
          _ => '.',
        };

        map_hash.insert(coord, ch);
      }
    }

    for goblin in self.goblins.clone() {
      map_hash.insert(goblin.coord, 'G');
    }

    for elf in self.elves.clone() {
      map_hash.insert(elf.coord, 'E');
    }

    for y in 0..self.map.dimensions.height {
      let mut line: Vec<char> = vec![];

      for x in 0..self.map.dimensions.width {
        let val = map_hash.get(&Coord { x: x, y: y }).unwrap();

        line.push(*val);
      }

      lines.push(line);
    }

    lines
      .iter()
      .map(|x| String::from_iter(x))
      .collect::<Vec<String>>()
      .join("\n")
  }

  fn print_game_str(&self) {
    let game_str = self.generate_str();

    println!("");
    for line in game_str.lines() {
      println!("{}", line);
    }
    println!(
      "goblins: {:?}",
      self
        .goblins
        .iter()
        .map(|x| x.hit_points)
        .collect::<Vec<usize>>()
    );
    println!(
      "elves: {:?}",
      self
        .elves
        .iter()
        .map(|x| x.hit_points)
        .collect::<Vec<usize>>()
    );
  }

  fn get_all_units_cloned(&self) -> Vec<Unit> {
    let mut all_units: Vec<Unit> = self.goblins.clone();

    all_units.extend(self.elves.clone());

    all_units
  }

  fn set_obstacles(&mut self) {
    let all_units: Vec<Unit> = self.get_all_units_cloned();
    let all_units_coords: Vec<Coord> = all_units.iter().map(|x| x.coord).collect();

    self.map.set_all_obstacles(&all_units_coords);
  }

  fn move_unit_to_pos(&mut self, unit_copy: &mut Unit, coord: &Coord) {
    unit_copy.move_to_pos(&coord);

    fn update_list(list: &mut Vec<Unit>, unit_copy: &Unit, coord: &Coord) {
      for item in list {
        if item.id == unit_copy.id {
          item.coord.x = coord.x;
          item.coord.y = coord.y;

          break;
        }
      }
    }

    match unit_copy.unit_type {
      UnitType::Goblin => update_list(&mut self.goblins, unit_copy, coord),
      UnitType::Elf => update_list(&mut self.elves, unit_copy, coord),
    };

    self.set_obstacles();
  }

  fn damage_unit(&mut self, unit_id: usize, attack_power: usize) {
    let all_units = self.get_all_units_cloned();

    fn update_list(list: &mut Vec<Unit>, unit_id: usize, attack_power: usize) {
      let mut should_remove_item = false;
      let mut item_idx = 0;

      for (idx, mut item) in list.iter_mut().enumerate() {
        if item.id == unit_id {
          if item.hit_points > attack_power {
            item.hit_points -= attack_power;
          } else {
            should_remove_item = true;
            item_idx = idx;
          }

          break;
        }
      }

      if should_remove_item {
        list.remove(item_idx);
      }
    }

    for item in all_units {
      if item.id == unit_id {
        match item.unit_type {
          UnitType::Goblin => update_list(&mut self.goblins, unit_id, attack_power),
          UnitType::Elf => update_list(&mut self.elves, unit_id, attack_power),
        };

        break;
      }
    }

    self.set_obstacles();
  }

  fn get_maybe_unit_idx(all_units: &Vec<Unit>, unit_id: usize) -> Option<usize> {
    let mut maybe_unit_idx = None;

    for (idx, unit) in all_units.iter().enumerate() {
      if unit.id == unit_id {
        maybe_unit_idx = Some(idx);
      }
    }

    maybe_unit_idx
  }

  fn perform_unit_move(&mut self, unit_id: usize) {
    let all_units = self.get_all_units_cloned();
    let maybe_unit_idx = Game::get_maybe_unit_idx(&all_units, unit_id);

    if maybe_unit_idx.is_none() {
      return;
    }

    let unit_idx = maybe_unit_idx.unwrap();

    let mut unit = all_units[unit_idx];

    let enemies_units = Unit::get_enemies(&unit, &all_units);

    if enemies_units.len() == 0 {
      self.has_finished = true;
      return;
    }

    let chosen_movement_pos = unit.get_chosen_movement_pos(&enemies_units, &self.map);

    if chosen_movement_pos.is_some() {
      let chosen_movement_coord = chosen_movement_pos.unwrap();

      self.move_unit_to_pos(&mut unit, &chosen_movement_coord);
    }
  }

  fn perform_unit_attack(&mut self, unit_id: usize) {
    let all_units = self.get_all_units_cloned();
    let maybe_unit_idx = Game::get_maybe_unit_idx(&all_units, unit_id);

    if maybe_unit_idx.is_none() {
      return;
    }

    let unit_idx = maybe_unit_idx.unwrap();
    let unit = all_units[unit_idx];

    let enemies_units = Unit::get_enemies(&unit, &all_units);
    let enemy_to_attack_id = unit.get_enemy_to_attack_id(&enemies_units);

    if enemy_to_attack_id.is_none() {
      return;
    }

    self.damage_unit(enemy_to_attack_id.unwrap(), unit.attack_power);
  }

  fn run_round(&mut self) {
    let mut all_units: Vec<Unit> = self.get_all_units_cloned();

    all_units.sort_by(Unit::sort_by_coord);

    let order_ids = all_units.iter().map(|x| x.id).collect::<Vec<usize>>();

    for unit_id in order_ids {
      self.perform_unit_move(unit_id);

      if self.has_finished == true {
        break;
      }

      self.perform_unit_attack(unit_id);
    }
  }

  pub fn run(&mut self) -> (usize, usize, bool) {
    let mut round_number = 0;
    let hit_points_result;

    fn get_hit_points_result(list: &Vec<Unit>) -> usize {
      list.iter().fold(0, |sum, unit| sum + unit.hit_points)
    }

    let mut did_elves_win = false;

    loop {
      self.run_round();

      if self.has_finished == true {
        if self.goblins.len() == 0 {
          hit_points_result = get_hit_points_result(&self.elves);
          did_elves_win = true;
          break;
        }

        if self.elves.len() == 0 {
          hit_points_result = get_hit_points_result(&self.goblins);
          break;
        }
      }

      round_number += 1;
    }

    (round_number, hit_points_result, did_elves_win)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data_1() -> String {
    "#######
#.G.E.#
#E.G.E#
#.G.E.#
#######"
      .to_string()
  }

  fn get_example_data_2() -> String {
    "#######
#E..G.#
#...#.#
#.G.#G#
#######"
      .to_string()
  }

  fn get_example_data_3() -> String {
    "#######
#.E...#
#.....#
#...G.#
#######"
      .to_string()
  }

  fn get_example_data_4() -> Vec<String> {
    vec![
      "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########"
        .to_string(),
      "#########
#.G...G.#
#...G...#
#...E..G#
#.G.....#
#.......#
#G..G..G#
#.......#
#########"
        .to_string(),
      "#########
#..G.G..#
#...G...#
#.G.E.G.#
#.......#
#G..G..G#
#.......#
#.......#
#########"
        .to_string(),
      "#########
#.......#
#..GGG..#
#..GEG..#
#G..G...#
#......G#
#.......#
#.......#
#########"
        .to_string(),
    ]
  }

  fn get_example_data_5() -> Vec<(String, usize, usize)> {
    vec![
      (
        "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"
          .to_string(),
        47,
        590,
      ),
      (
        "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"
          .to_string(),
        37,
        982,
      ),
      (
        "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"
          .to_string(),
        46,
        859,
      ),
    ]
  }

  #[test]
  fn test_game_new() {
    let text = get_example_data_1();
    let mut game = Game::new(text, None);

    assert_eq!(game.rounds, 0);
    assert_eq!(game.goblins.iter().count(), 3);
    assert_eq!(game.elves.iter().count(), 4);
    assert_eq!(game.goblins[0].get_is_in_range(&mut game.elves), false);

    game.goblins[0].coord.x += 1;
    assert_eq!(game.goblins[0].get_is_in_range(&mut game.elves), true);
    game.goblins[0].coord.x -= 1;
  }

  #[test]
  fn test_get_chosen_target_1() {
    let expected_results = vec![
      (get_example_data_2(), Coord { x: 2, y: 1 }),
      (get_example_data_3(), Coord { x: 3, y: 1 }),
    ];
    for expected_result in expected_results {
      let mut game = Game::new(expected_result.0, None);
      let mut all_units: Vec<Unit> = game.get_all_units_cloned();
      let elf = game.elves[0];
      let enemies_units = Unit::get_enemies(&elf, &all_units);
      let chosen_movement_pos = elf
        .get_chosen_movement_pos(&enemies_units, &game.map)
        .unwrap();

      assert_eq!(chosen_movement_pos, expected_result.1);
    }
  }

  #[test]
  fn test_movement_1() {
    let rounds_data = get_example_data_4();
    let mut game = Game::new(rounds_data.get(0).unwrap().clone(), None);

    for round in 1..rounds_data.len() {
      game.run_round();

      let expected_result = rounds_data.get(round).unwrap().clone();

      assert_eq!(game.generate_str(), expected_result);
    }
  }

  #[test]
  fn test_rounds() {
    let test_datas = get_example_data_5();
    for test_data in test_datas {
      let mut game = Game::new(test_data.0, None);

      let (rounds_num, hit_points_sum, _) = game.run();

      assert_eq!(rounds_num, test_data.1);
      assert_eq!(hit_points_sum, test_data.2);
    }
  }
}
