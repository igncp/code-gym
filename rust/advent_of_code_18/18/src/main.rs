/*

--- Day 18: Settlers of The North Pole ---

On the outskirts of the North Pole base construction project, many Elves are collecting lumber.

The lumber collection area is 50 acres by 50 acres; each acre can be either open ground (.), trees
(|), or a lumberyard (#). You take a scan of the area (your puzzle input).

Strange magic is at work here: each minute, the landscape looks entirely different. In exactly one
minute, an open acre can fill with trees, a wooded acre can be converted to a lumberyard, or a
lumberyard can be cleared to open ground (the lumber having been sent to other projects).

The change to each acre is based entirely on the contents of that acre as well as the number of
open, wooded, or lumberyard acres adjacent to it at the start of each minute. Here, "adjacent"
means any of the eight acres surrounding that acre. (Acres on the edges of the lumber collection
area might have fewer than eight adjacent acres; the missing acres aren't counted.)

In particular:

An open acre will become filled with trees if three or more adjacent acres contained trees.
Otherwise, nothing happens.

An acre filled with trees will become a lumberyard if three or more adjacent acres were
lumberyards. Otherwise, nothing happens.

An acre containing a lumberyard will remain a lumberyard if it was adjacent to at least one other
lumberyard and at least one acre containing trees. Otherwise, it becomes open.

These changes happen across all acres simultaneously, each of them using the state of all acres at
the beginning of the minute and changing to their new form by the end of that same minute. Changes
that happen during the minute don't affect each other.

For example, suppose the lumber collection area is instead only 10 by 10 acres with this initial
configuration:

Initial state:

.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.

After 1 minute:

.......##.
......|###
.|..|...#.
..|#||...#
..##||.|#|
...#||||..
||...|||..
|||||.||.|
||||||||||
....||..|.

After 2 minutes:

.......#..
......|#..
.|.|||....
..##|||..#
..###|||#|
...#|||||.
|||||||||.
||||||||||
||||||||||
.|||||||||

After 3 minutes:

.......#..
....|||#..
.|.||||...
..###|||.#
...##|||#|
.||##|||||
||||||||||
||||||||||
||||||||||
||||||||||

After 4 minutes:

.....|.#..
...||||#..
.|.#||||..
..###||||#
...###||#|
|||##|||||
||||||||||
||||||||||
||||||||||
||||||||||

After 5 minutes:

....|||#..
...||||#..
.|.##||||.
..####|||#
.|.###||#|
|||###||||
||||||||||
||||||||||
||||||||||
||||||||||

After 6 minutes:

...||||#..
...||||#..
.|.###|||.
..#.##|||#
|||#.##|#|
|||###||||
||||#|||||
||||||||||
||||||||||
||||||||||

After 7 minutes:

...||||#..
..||#|##..
.|.####||.
||#..##||#
||##.##|#|
|||####|||
|||###||||
||||||||||
||||||||||
||||||||||

After 8 minutes:

..||||##..
..|#####..
|||#####|.
||#...##|#
||##..###|
||##.###||
|||####|||
||||#|||||
||||||||||
||||||||||

After 9 minutes:

..||###...
.||#####..
||##...##.
||#....###
|##....##|
||##..###|
||######||
|||###||||
||||||||||
||||||||||

After 10 minutes:

.||##.....
||###.....
||##......
|##.....##
|##.....##
|##....##|
||##.####|
||#####|||
||||#|||||
||||||||||

After 10 minutes, there are 37 wooded acres and 31 lumberyards. Multiplying the number of wooded
acres by the number of lumberyards gives the total resource value after ten minutes: 37 * 31 =
1147.

What will the total resource value of the lumber collection area be after 10 minutes?

*/

extern crate md5;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

type LengthUnit = usize;

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
struct Coord {
  x: LengthUnit,
  y: LengthUnit,
}

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
struct Boundary {
  max_x: LengthUnit,
  max_y: LengthUnit,
}

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
enum TerrainType {
  Ground,
  Trees,
  Lumberyard,
}

type MapTopology = HashMap<Coord, TerrainType>;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Map {
  topology: MapTopology,
  boundary: Boundary,
  surrounding_coords: HashMap<Coord, HashSet<Coord>>,
}

impl Map {
  fn get_surrounding_coords(coord: &Coord, boundary: &Boundary) -> HashSet<Coord> {
    let mut coords: HashSet<Coord> = HashSet::new();
    let x = coord.x;
    let y = coord.y;

    if coord.x != 0 {
      coords.insert(Coord { x: x - 1, y });

      if coord.y != 0 {
        coords.insert(Coord { x: x - 1, y: y - 1 });
      }

      if coord.y != boundary.max_y {
        coords.insert(Coord { x: x - 1, y: y + 1 });
      }
    }

    if coord.x != boundary.max_y {
      coords.insert(Coord { x: x + 1, y });

      if coord.y != 0 {
        coords.insert(Coord { x: x + 1, y: y - 1 });
      }

      if coord.y != boundary.max_y {
        coords.insert(Coord { x: x + 1, y: y + 1 });
      }
    }

    if coord.y != 0 {
      coords.insert(Coord { x, y: y - 1 });
    }

    if coord.y != boundary.max_y {
      coords.insert(Coord { x, y: y + 1 });
    }

    coords
  }

  fn from_string(text: &str) -> Self {
    let chars: Vec<Vec<char>> = text.lines().map(|x| x.chars().collect()).collect();
    let mut topology: MapTopology = HashMap::new();
    let mut boundary = Boundary { max_x: 0, max_y: 0 };
    let mut surrounding_coords: HashMap<Coord, HashSet<Coord>> = HashMap::new();

    for (y, line) in chars.iter().enumerate() {
      for (x, ch) in line.iter().enumerate() {
        let coord = Coord { x, y };
        let terrain_type = match ch {
          '.' => Some(TerrainType::Ground),
          '#' => Some(TerrainType::Lumberyard),
          '|' => Some(TerrainType::Trees),
          _ => None,
        };

        topology.insert(coord, terrain_type.unwrap());
        boundary.max_x = x;
      }

      boundary.max_y = y;
    }

    for y in 0..=boundary.max_y {
      for x in 0..=boundary.max_x {
        let coord = Coord { x, y };
        let surrounding_coords_val = Map::get_surrounding_coords(&coord, &boundary);

        surrounding_coords.insert(coord, surrounding_coords_val);
      }
    }

    Map {
      topology,
      boundary,
      surrounding_coords,
    }
  }

  fn evolve_one_minute(&mut self) {
    let orig_state = self.topology.clone();

    for y in 0..=self.boundary.max_y {
      for x in 0..=self.boundary.max_x {
        let coord = Coord { x, y };

        let new_val = match self.topology[&coord] {
          TerrainType::Ground => {
            let mut found_trees = 0;
            for other_coord in Map::get_surrounding_coords(&coord, &self.boundary).iter() {
              if orig_state[other_coord] == TerrainType::Trees {
                found_trees += 1;
              }
            }

            if found_trees > 2 {
              Some(TerrainType::Trees)
            } else {
              Some(TerrainType::Ground)
            }
          }
          TerrainType::Trees => {
            let mut found_lumberyards = 0;
            for other_coord in Map::get_surrounding_coords(&coord, &self.boundary).iter() {
              if orig_state[other_coord] == TerrainType::Lumberyard {
                found_lumberyards += 1;
              }
            }

            if found_lumberyards > 2 {
              Some(TerrainType::Lumberyard)
            } else {
              Some(TerrainType::Trees)
            }
          }
          TerrainType::Lumberyard => {
            let mut found_trees = 0;;
            let mut found_lumberyards = 0;

            for other_coord in Map::get_surrounding_coords(&coord, &self.boundary).iter() {
              if orig_state[other_coord] == TerrainType::Trees {
                found_trees += 1;
              } else if orig_state[other_coord] == TerrainType::Lumberyard {
                found_lumberyards += 1
              }
            }

            if found_lumberyards > 0 && found_trees > 0 {
              Some(TerrainType::Lumberyard)
            } else {
              Some(TerrainType::Ground)
            }
          }
        };

        self.topology.insert(coord, new_val.unwrap());
      }
    }
  }

  fn serialize(&self) -> md5::Digest {
    use std::iter::FromIterator;

    let mut chars: Vec<char> = vec![];

    for y in 0..=self.boundary.max_y {
      for x in 0..=self.boundary.max_x {
        let coord = Coord { x, y };

        let ch = match self.topology[&coord] {
          TerrainType::Ground => '.',
          TerrainType::Trees => '|',
          TerrainType::Lumberyard => '#',
        };

        chars.push(ch);
      }
    }

    let full_str = String::from_iter(chars);

    md5::compute(full_str)
  }

  fn evolve_n_minutes_with_history(&mut self, minutes: usize) {
    let mut history: HashMap<md5::Digest, usize> = HashMap::new();

    for minute in 0..minutes {
      let serialized = self.serialize();

      if history.get(&serialized).is_some() {
        let num = history[&serialized];

        let interval = minute - num;
        let remaining_total = minutes - minute;
        let remainder = remaining_total % interval;

        for _ in 0..remainder {
          self.evolve_one_minute();
        }

        break;
      }

      history.insert(serialized, minute);

      self.evolve_one_minute();
    }
  }

  fn count_resource_value(&self) -> usize {
    let mut lumberyards = 0;
    let mut trees = 0;

    for y in 0..=self.boundary.max_y {
      for x in 0..=self.boundary.max_x {
        let coord = Coord { x, y };

        match self.topology[&coord] {
          TerrainType::Lumberyard => {
            lumberyards += 1;
          }
          TerrainType::Trees => {
            trees += 1;
          }
          _ => {}
        }
      }
    }

    lumberyards * trees
  }
}

impl std::fmt::Display for Map {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    for y in 0..=self.boundary.max_y {
      for x in 0..=self.boundary.max_x {
        let coord = Coord { x, y };
        let val = match self.topology[&coord] {
          TerrainType::Ground => '.',
          TerrainType::Lumberyard => '#',
          TerrainType::Trees => '|',
        };

        write!(f, "{}", val)?;
      }

      writeln!(f)?;
    }

    Ok(())
  }
}

fn get_input_map() -> Map {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  Map::from_string(&contents)
}

fn main() {
  let mut map = get_input_map();
  let mut map_2 = get_input_map();

  map.evolve_n_minutes_with_history(10);
  map_2.evolve_n_minutes_with_history(1_000_000_000);

  println!("Results");
  println!("- (1) resource value: {}", map.count_resource_value());
  println!("- (2) resource value: {}", map_2.count_resource_value());
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data() -> String {
    ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."
      .to_string()
  }

  fn get_example_data_1() -> String {
    ".......##.
......|###
.|..|...#.
..|#||...#
..##||.|#|
...#||||..
||...|||..
|||||.||.|
||||||||||
....||..|."
      .to_string()
  }

  #[test]
  fn test_map_from_string() {
    let text = get_example_data();
    let result = Map::from_string(&text);

    assert_eq!(
      result.topology[&Coord { x: 1, y: 0 }],
      TerrainType::Lumberyard
    );
  }

  #[test]
  fn test_map_evolve_one_minute() {
    let text = get_example_data();
    let text_2 = get_example_data_1();

    let mut result = Map::from_string(&text);
    let result_2 = Map::from_string(&text_2);

    result.evolve_one_minute();

    assert_eq!(result, result_2);
  }

  #[test]
  fn test_example_1() {
    let text = get_example_data();
    let mut result = Map::from_string(&text);

    result.evolve_n_minutes_with_history(10);

    assert_eq!(result.count_resource_value(), 1147);
  }
}
