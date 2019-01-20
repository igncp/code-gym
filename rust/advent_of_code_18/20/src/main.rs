/*

--- Day 20: A Regular Map ---

While you were learning about instruction pointers, the Elves made considerable progress. When you
look up, you discover that the North Pole base construction project has completely surrounded you.

The area you are in is made up entirely of rooms and doors. The rooms are arranged in a grid, and
rooms only connect to adjacent rooms when a door is present between them.

For example, drawing rooms as ., walls as #, doors as | or -, your current position as X, and where
north is up, the area you're in might look like this:

#####
#.|.#
#-###
#.|X#
#####

You get the attention of a passing construction Elf and ask for a map. "I don't have time to draw
out a map of this place - it's huge. Instead, I can give you directions to every room in the
facility!" He writes down some directions on a piece of parchment and runs off. In the example
above, the instructions might have been ^WNE$, a regular expression or "regex" (your puzzle input).

The regex matches routes (like WNE for "west, north, east") that will take you from your current
room through various doors in the facility. In aggregate, the routes will take you through every
door in the facility at least once; mapping out all of these routes will let you build a proper map
and find your way around.

^ and $ are at the beginning and end of your regex; these just mean that the regex doesn't match
anything outside the routes it describes. (Specifically, ^ matches the start of the route, and $
matches the end of it.) These characters will not appear elsewhere in the regex.

The rest of the regex matches various sequences of the characters N (north), S (south), E (east),
and W (west). In the example above, ^WNE$ matches only one route, WNE, which means you can move
west, then north, then east from your current position. Sequences of letters like this always match
that exact route in the same order.

Sometimes, the route can branch. A branch is given by a list of options separated by pipes (|) and
wrapped in parentheses. So, ^N(E|W)N$ contains a branch: after going north, you must choose to go
either east or west before finishing your route by going north again. By tracing out the possible
routes after branching, you can determine where the doors are and, therefore, where the rooms are
in the facility.

For example, consider this regex: ^ENWWW(NEEE|SSE(EE|N))$

This regex begins with ENWWW, which means that from your current position, all routes must begin by
moving east, north, and then west three times, in that order. After this, there is a branch. Before
you consider the branch, this is what you know about the map so far, with doors you aren't sure
about marked with a ?:

#?#?#?#?#
?.|.|.|.?
#?#?#?#-#
    ?X|.?
    #?#?#

After this point, there is (NEEE|SSE(EE|N)). This gives you exactly two options: NEEE and
SSE(EE|N). By following NEEE, the map now looks like this:

#?#?#?#?#
?.|.|.|.?
#-#?#?#?#
?.|.|.|.?
#?#?#?#-#
    ?X|.?
    #?#?#

Now, only SSE(EE|N) remains. Because it is in the same parenthesized group as NEEE, it starts from
the same room NEEE started in. It states that starting from that point, there exist doors which
will allow you to move south twice, then east; this ends up at another branch. After that, you can
either move east twice or north once. This information fills in the rest of the doors:

#?#?#?#?#
?.|.|.|.?
#-#?#?#?#
?.|.|.|.?
#-#?#?#-#
?.?.?X|.?
#-#-#?#?#
?.|.|.|.?
#?#?#?#?#

Once you've followed all possible routes, you know the remaining unknown parts are all walls,
producing a finished map of the facility:

#########
#.|.|.|.#
#-#######
#.|.|.|.#
#-#####-#
#.#.#X|.#
#-#-#####
#.|.|.|.#
#########

Sometimes, a list of options can have an empty option, like (NEWS|WNSE|). This means that routes at
this point could effectively skip the options in parentheses and move on immediately. For example,
consider this regex and the corresponding map:

^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$

###########
#.|.#.|.#.#
#-###-#-#-#
#.|.|.#.#.#
#-#####-#-#
#.#.#X|.#.#
#-#-#####-#
#.#.|.|.|.#
#-###-###-#
#.|.|.#.|.#
###########

This regex has one main route which, at three locations, can optionally include additional detours
and be valid: (NEWS|), (WNSE|), and (SWEN|). Regardless of which option is taken, the route
continues from the position it is left at after taking those steps. So, for example, this regex
matches all of the following routes (and more that aren't listed here):

ENNWSWWSSSEENEENNN
ENNWSWWNEWSSSSEENEENNN
ENNWSWWNEWSSSSEENEESWENNNN
ENNWSWWSSSEENWNSEEENNN

By following the various routes the regex matches, a full map of all of the doors and rooms in the
facility can be assembled.

To get a sense for the size of this facility, you'd like to determine which room is furthest from
you: specifically, you would like to find the room for which the shortest path to that room would
require passing through the most doors.

In the first example (^WNE$), this would be the north-east corner 3 doors away.

In the second example (^ENWWW(NEEE|SSE(EE|N))$), this would be the south-east corner 10 doors away.

In the third example (^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$), this would be the north-east
corner 18 doors away.

Here are a few more examples:

Regex: ^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$

Furthest room requires passing 23 doors

#############
#.|.|.|.|.|.#
#-#####-###-#
#.#.|.#.#.#.#
#-#-###-#-#-#
#.#.#.|.#.|.#
#-#-#-#####-#
#.#.#.#X|.#.#
#-#-#-###-#-#
#.|.#.|.#.#.#
###-#-###-#-#
#.|.#.|.|.#.#
#############

Regex: ^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$
Furthest room requires passing 31 doors

###############
#.|.|.|.#.|.|.#
#-###-###-#-#-#
#.|.#.|.|.#.#.#
#-#########-#-#
#.#.|.|.|.|.#.#
#-#-#########-#
#.#.#.|X#.|.#.#
###-#-###-#-#-#
#.|.#.#.|.#.|.#
#-###-#####-###
#.|.#.|.|.#.#.#
#-#-#####-#-#-#
#.#.|.|.|.#.|.#
###############

What is the largest number of doors you would be required to pass through to reach a room?
That is, find the room for which the shortest path from your starting location to that room would
require passing through the most doors; what is the fewest doors you can pass through to reach it?

--- Part Two ---

Okay, so the facility is big.

How many rooms have a shortest path from your current location that pass through at least 1000
doors?

*/

extern crate pathfinding;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;

use pathfinding::directed::dijkstra::dijkstra_all;

type LengthUnit = i32;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct Coord {
  x: LengthUnit,
  y: LengthUnit,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct Boundary {
  min_x: LengthUnit,
  max_x: LengthUnit,
  min_y: LengthUnit,
  max_y: LengthUnit,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum TerrainType {
  Room,
  Door,
  Wall,
}

type MapTopology = HashMap<Coord, TerrainType>;

#[derive(Debug)]
struct Map {
  topology: MapTopology,
  boundary: Boundary,
  directions: String,
}

impl Map {
  fn add_topology_terrain(&mut self, current_coord: &Coord, ch: char) -> Coord {
    let coords = match ch {
      'E' => Some((
        Coord {
          x: current_coord.x + 1,
          y: current_coord.y,
        },
        Coord {
          x: current_coord.x + 2,
          y: current_coord.y,
        },
      )),
      'W' => Some((
        Coord {
          x: current_coord.x - 1,
          y: current_coord.y,
        },
        Coord {
          x: current_coord.x - 2,
          y: current_coord.y,
        },
      )),
      'N' => Some((
        Coord {
          x: current_coord.x,
          y: current_coord.y - 1,
        },
        Coord {
          x: current_coord.x,
          y: current_coord.y - 2,
        },
      )),
      'S' => Some((
        Coord {
          x: current_coord.x,
          y: current_coord.y + 1,
        },
        Coord {
          x: current_coord.x,
          y: current_coord.y + 2,
        },
      )),
      _ => None,
    };

    let (door_coord, next_coord) = coords.unwrap();

    self.topology.insert(next_coord, TerrainType::Room);
    self.topology.insert(door_coord, TerrainType::Door);

    next_coord
  }

  fn new_from_directions_str(directions: &str) -> Self {
    let mut topology: MapTopology = HashMap::new();
    let chs: Vec<char> = directions.chars().collect();
    let mut current_coord = Coord { x: 0, y: 0 };
    let mut paths: Vec<Coord> = vec![];
    let boundary = Boundary {
      max_x: 0,
      max_y: 0,
      min_x: 0,
      min_y: 0,
    };

    topology.insert(current_coord, TerrainType::Room);

    let mut map = Map {
      topology,
      boundary,
      directions: directions.to_string(),
    };

    for ch in chs {
      match ch {
        'N' | 'S' | 'E' | 'W' => {
          current_coord = map.add_topology_terrain(&current_coord, ch);
        }
        '(' => {
          paths.push(current_coord);
        }
        ')' => {
          current_coord = paths.pop().unwrap();
        }
        '|' => {
          current_coord = *paths.last().unwrap();
        }
        _ => {}
      }
    }

    map.apply_walls();

    map
  }

  fn get_topology_boundary(&self) -> Boundary {
    let coords: Vec<&Coord> = self.topology.keys().collect();

    let mut boundary = Boundary {
      min_x: coords[0].x,
      max_x: coords[0].x,
      min_y: coords[0].y,
      max_y: coords[0].y,
    };

    for coord in coords {
      if coord.x > boundary.max_x {
        boundary.max_x = coord.x;
      }
      if coord.x < boundary.min_x {
        boundary.min_x = coord.x;
      }
      if coord.y > boundary.max_y {
        boundary.max_y = coord.y;
      }
      if coord.y < boundary.min_y {
        boundary.min_y = coord.y;
      }
    }

    boundary
  }

  fn apply_walls(&mut self) {
    let boundary = self.get_topology_boundary();

    let min_y = boundary.min_y - 1;
    let max_y = boundary.max_y + 1;
    let min_x = boundary.min_x - 1;
    let max_x = boundary.max_x + 1;

    for y in min_y..=max_y {
      for x in min_x..=max_x {
        let coord = Coord { x, y };
        if self.topology.get(&coord).is_none() {
          self.topology.insert(coord, TerrainType::Wall);
        }
      }
    }

    self.boundary = Boundary {
      max_x,
      min_x,
      max_y,
      min_y,
    };
  }

  fn get_door_char(&self, coord: &Coord) -> char {
    let coord_above = Coord {
      x: coord.x,
      y: coord.y - 1,
    };
    let terrain_type = self.topology.get(&coord_above);

    if terrain_type.is_some() && *terrain_type.unwrap() == TerrainType::Room {
      return '-';
    }

    '|'
  }

  fn get_part_1_and_2(&self) -> (usize, usize) {
    let min_doors_to_count = 1000;
    let starting_coord = Coord { x: 0, y: 0 };

    fn get_successors(map: &Map, coord: &Coord) -> Vec<(Coord, usize)> {
      let next_rooms = map.get_rooms_next_to_coord(coord);
      let mut successors: Vec<(Coord, usize)> = vec![];

      for next_room in next_rooms {
        successors.push((next_room, 1))
      }

      successors
    }

    let all_reachable_rooms = dijkstra_all(&starting_coord, |x| get_successors(&self, &x));

    let mut part_1 = 0;
    let mut part_2 = 0;

    for coord in all_reachable_rooms.clone().keys() {
      let doors_num = all_reachable_rooms[coord].1;

      if part_1 < doors_num {
        part_1 = doors_num;
      }

      if doors_num >= min_doors_to_count {
        part_2 += 1;
      }
    }

    (part_1, part_2)
  }

  fn get_rooms_next_to_coord(&self, coord: &Coord) -> HashSet<Coord> {
    let mut rooms: HashSet<Coord> = HashSet::new();

    let coord_above = Coord {
      x: coord.x,
      y: coord.y - 1,
    };
    let coord_below = Coord {
      x: coord.x,
      y: coord.y + 1,
    };
    let coord_left = Coord {
      x: coord.x - 1,
      y: coord.y,
    };
    let coord_right = Coord {
      x: coord.x + 1,
      y: coord.y,
    };

    if self.topology.get(&coord_above) == Some(&TerrainType::Door) {
      rooms.insert(Coord {
        x: coord.x,
        y: coord_above.y - 1,
      });
    }
    if self.topology.get(&coord_below) == Some(&TerrainType::Door) {
      rooms.insert(Coord {
        x: coord.x,
        y: coord_below.y + 1,
      });
    }
    if self.topology.get(&coord_left) == Some(&TerrainType::Door) {
      rooms.insert(Coord {
        x: coord_left.x - 1,
        y: coord.y,
      });
    }
    if self.topology.get(&coord_right) == Some(&TerrainType::Door) {
      rooms.insert(Coord {
        x: coord_right.x + 1,
        y: coord.y,
      });
    }

    rooms
  }

  fn get_representation(&self) -> String {
    let mut lines: Vec<String> = vec![];

    for y in self.boundary.min_y..=self.boundary.max_y {
      let mut line: Vec<char> = vec![];

      for x in self.boundary.min_x..=self.boundary.max_x {
        let coord = Coord { x, y };
        let mut ch = 'X';
        if coord.x != 0 || coord.y != 0 {
          ch = match self.topology.get(&coord).unwrap() {
            TerrainType::Wall => '#',
            TerrainType::Door => self.get_door_char(&coord),
            TerrainType::Room => '.',
          };
        }

        line.push(ch);
      }

      lines.push(String::from_iter(line));
    }

    lines.join("\n")
  }
}

impl std::fmt::Display for Map {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    let representation = self.get_representation();

    for line in representation.lines() {
      writeln!(f, "{}", line)?;
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

  Map::new_from_directions_str(&contents)
}

fn main() {
  let map = get_input_map();
  let (min_doors, rooms_num) = map.get_part_1_and_2();

  println!("Results:");
  println!("- (1) min doors for longest path: {}", min_doors);
  println!("- (2) rooms number: {}", rooms_num);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data_1() -> Map {
    Map::new_from_directions_str("^WNE$")
  }

  fn get_example_data_2() -> Map {
    Map::new_from_directions_str("^ENWWW(NEEE|SSE(EE|N))$")
  }

  #[test]
  fn test_get_representation_1() {
    assert_eq!(
      get_example_data_1().get_representation(),
      "#####
#.|.#
#-###
#.|X#
#####"
    );
  }

  #[test]
  fn test_get_representation_2() {
    assert_eq!(
      get_example_data_2().get_representation(),
      "#########
#.|.|.|.#
#-#######
#.|.|.|.#
#-#####-#
#.#.#X|.#
#-#-#####
#.|.|.|.#
#########"
    );
  }

  #[test]
  fn test_get_part_1_and_2() {
    assert_eq!(get_example_data_1().get_part_1_and_2().0, 3);
    assert_eq!(get_example_data_2().get_part_1_and_2().0, 10);
    assert_eq!(
      Map::new_from_directions_str("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
        .get_part_1_and_2()
        .0,
      18
    );
    assert_eq!(
      Map::new_from_directions_str("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
        .get_part_1_and_2()
        .0,
      23
    );
  }
}
