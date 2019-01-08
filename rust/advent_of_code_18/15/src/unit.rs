use std::cmp::Ordering;
use std::collections::HashSet;

use pathfinding::directed::astar::astar_bag;
use pathfinding::directed::dijkstra::dijkstra_all;

use space::{Coord, Map};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnitType {
  Elf,
  Goblin,
}

#[derive(Debug, Clone, Copy)]
pub struct Unit {
  pub attack_power: usize,
  pub id: usize,
  pub hit_points: usize,
  pub unit_type: UnitType,
  pub coord: Coord,
}

impl Unit {
  pub fn get_units(chars_vecs: &[Vec<char>]) -> (Vec<Unit>, Vec<Unit>) {
    let mut goblins: Vec<Unit> = vec![];
    let mut elves: Vec<Unit> = vec![];

    let mut unit_id_counter = 0;

    for (y, line) in chars_vecs.iter().enumerate() {
      for (x, ch) in line.iter().enumerate() {
        let coord = Coord::new(x, y);

        if ch == &'G' || ch == &'E' {
          let mut unit = Unit {
            coord,
            id: unit_id_counter,
            attack_power: 3,
            hit_points: 200,
            unit_type: UnitType::Goblin,
          };

          unit_id_counter += 1;

          if ch == &'G' {
            goblins.push(unit);
          } else {
            unit.unit_type = UnitType::Elf;
            elves.push(unit);
          }
        }
      }
    }

    (goblins, elves)
  }

  pub fn sort_by_coord(a: &Self, b: &Self) -> Ordering {
    a.coord.cmp(&b.coord)
  }

  pub fn get_enemies(unit: &Self, all_units: &[Self]) -> Vec<Self> {
    let mut cloned = all_units.to_owned();
    cloned.retain(|x| x.unit_type != unit.unit_type);
    cloned
  }

  fn get_successors(coord: &Coord, map: &Map) -> Vec<(Coord, usize)> {
    let coords = vec![*coord];
    let ranges = map.get_all_ranges(&coords);
    let mut successors: Vec<(Coord, usize)> = vec![];

    for range in ranges {
      successors.push((range, 1))
    }

    successors
  }

  pub fn get_ranges_for_units(all_units: &[Self], map: &Map) -> HashSet<Coord> {
    let all_units_coords: Vec<Coord> = all_units.iter().map(|x| x.coord).collect();

    map.get_all_ranges(&all_units_coords)
  }

  pub fn move_to_pos(&mut self, pos: &Coord) {
    self.coord.x = pos.x;
    self.coord.y = pos.y;
  }

  pub fn get_chosen_movement_pos(&self, target_units: &[Unit], map: &Map) -> Option<Coord> {
    for target_unit in target_units {
      if self.coord.get_is_next_to_coord(&target_unit.coord) {
        return None;
      }
    }

    let ranges = Self::get_ranges_for_units(target_units, &map);
    let result = dijkstra_all(&self.coord, |x| Unit::get_successors(&x, &map));

    let mut smallest_cost: Option<usize> = None;
    let mut chosen_ranges: Vec<Coord> = vec![];
    for key in result.clone().keys() {
      let value = &result[key];

      if ranges.contains(&key) {
        if smallest_cost.is_none() || smallest_cost.unwrap() > value.1 {
          chosen_ranges = vec![*key];
          smallest_cost = Some(value.1);
        } else if smallest_cost.unwrap() == value.1 {
          chosen_ranges.push(*key);
        }
      }
    }

    if chosen_ranges.is_empty() {
      return None;
    }

    chosen_ranges.sort();

    let chosen_range = chosen_ranges[0];

    let paths = astar_bag(
      &self.coord,
      |x| Unit::get_successors(&x, &map),
      |_| 1,
      |x| x == &chosen_range,
    )
    .unwrap()
    .0;

    let mut possible_next_steps: Vec<Coord> = paths.map(|x| x[1]).collect();

    possible_next_steps.sort();

    Some(possible_next_steps[0])
  }

  pub fn get_enemy_to_attack_id(&self, enemies: &[Unit]) -> Option<usize> {
    let mut enemy_id: Option<usize> = None;
    let mut enemies_with_least_hp: Vec<Unit> = vec![];
    let mut lowest_found_hp: Option<usize> = None;

    let mut reachable_enemies = enemies.to_owned();

    reachable_enemies.retain(|x| x.coord.get_is_next_to_coord(&self.coord));

    for enemy in reachable_enemies {
      if lowest_found_hp.is_none() || enemy.hit_points < lowest_found_hp.unwrap() {
        lowest_found_hp = Some(enemy.hit_points);
        enemies_with_least_hp = vec![enemy];
      } else if enemy.hit_points == lowest_found_hp.unwrap() {
        enemies_with_least_hp.push(enemy);
      }
    }

    enemies_with_least_hp.sort_by(Unit::sort_by_coord);

    if !enemies_with_least_hp.is_empty() {
      enemy_id = Some(enemies_with_least_hp[0].id);
    }

    enemy_id
  }
}

#[cfg(test)]
mod tests {}
