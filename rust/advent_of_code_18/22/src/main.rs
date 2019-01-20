/*

--- Day 22: Mode Maze ---

This is it, your final stop: the year -483. It's snowing and dark outside; the only light you can
see is coming from a small cottage in the distance. You make your way there and knock on the door.

A portly man with a large, white beard answers the door and invites you inside. For someone living
near the North Pole in -483, he must not get many visitors, but he doesn't act surprised to see
you. Instead, he offers you some milk and cookies.

After talking for a while, he asks a favor of you. His friend hasn't come back in a few hours, and
he's not sure where he is. Scanning the region briefly, you discover one life signal in a cave
system nearby; his friend must have taken shelter there. The man asks if you can go there to
retrieve his friend.

The cave is divided into square regions which are either dominantly rocky, narrow, or wet (called
its type). Each region occupies exactly one coordinate in X,Y format where X and Y are integers and
zero or greater. (Adjacent regions can be the same type.)

The scan (your puzzle input) is not very detailed: it only reveals the depth of the cave system and
the coordinates of the target. However, it does not reveal the type of each region. The mouth of
the cave is at 0,0.

The man explains that due to the unusual geology in the area, there is a method to determine any
region's type based on its erosion level. The erosion level of a region can be determined from its
geologic index. The geologic index can be determined using the first rule that applies from the
list below:

The region at 0,0 (the mouth of the cave) has a geologic index of 0.

The region at the coordinates of the target has a geologic index of 0.

If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.

If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.

Otherwise, the region's geologic index is the result of multiplying the erosion levels of the
regions at X-1,Y and X,Y-1.

A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183. Then:

If the erosion level modulo 3 is 0, the region's type is rocky.
If the erosion level modulo 3 is 1, the region's type is wet.
If the erosion level modulo 3 is 2, the region's type is narrow.

For example, suppose the cave system's depth is 510 and the target's coordinates are 10,10. Using %
to represent the modulo operator, the cavern would look as follows:

At 0,0, the geologic index is 0. The erosion level is (0 + 510) % 20183 = 510. The type is 510 % 3
= 0, rocky.

At 1,0, because the Y coordinate is 0, the geologic index is 1 * 16807 = 16807. The erosion level
is (16807 + 510) % 20183 = 17317. The type is 17317 % 3 = 1, wet.

At 0,1, because the X coordinate is 0, the geologic index is 1 * 48271 = 48271. The erosion level
is (48271 + 510) % 20183 = 8415. The type is 8415 % 3 = 0, rocky.

At 1,1, neither coordinate is 0 and it is not the coordinate of the target, so the geologic index
is the erosion level of 0,1 (8415) times the erosion level of 1,0 (17317), 8415 * 17317 =
145722555. The erosion level is (145722555 + 510) % 20183 = 1805. The type is 1805 % 3 = 2, narrow.

At 10,10, because they are the target's coordinates, the geologic index is 0. The erosion level is
(0 + 510) % 20183 = 510. The type is 510 % 3 = 0, rocky.

Drawing this same cave system with rocky as ., wet as =, narrow as |, the mouth as M, the target as
T, with 0,0 in the top-left corner, X increasing to the right, and Y increasing downward, the
top-left corner of the map looks like this:

M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Before you go in, you should determine the risk level of the area. For the rectangle that has a
top-left corner of region 0,0 and a bottom-right corner of the region containing the target, add up
the risk level of each individual region: 0 for rocky regions, 1 for wet regions, and 2 for narrow
regions.

In the cave system above, because the mouth is at 0,0 and the target is at 10,10, adding up the
risk level of all regions with an X coordinate from 0 to 10 and a Y coordinate from 0 to 10, this
total is 114.

What is the total risk level for the smallest rectangle that includes 0,0 and the target's
coordinates?

--- Part Two ---

Okay, it's time to go rescue the man's friend.

As you leave, he hands you some tools: a torch and some climbing gear. You can't equip both tools
at once, but you can choose to use neither.

Tools can only be used in certain regions:

In rocky regions, you can use the climbing gear or the torch. You cannot use
neither (you'll likely slip and fall).

In wet regions, you can use the climbing gear or neither tool. You cannot use
the torch (if it gets wet, you won't have a light source).

In narrow regions, you can use the torch or neither tool. You cannot use the
climbing gear (it's too bulky to fit).

You start at 0,0 (the mouth of the cave) with the torch equipped and must reach
the target coordinates as quickly as possible. The regions with negative X or Y
are solid rock and cannot be traversed. The fastest route might involve
entering regions beyond the X or Y coordinate of the target.

You can move to an adjacent region (up, down, left, or right; never diagonally)
if your currently equipped tool allows you to enter that region. Moving to an
adjacent region takes one minute. (For example, if you have the torch equipped,
you can move between rocky and narrow regions, but cannot enter wet regions.)

You can change your currently equipped tool or put both away if your new
equipment would be valid for your current region. Switching to using the
climbing gear, torch, or neither always takes seven minutes, regardless of
which tools you start with. (For example, if you are in a rocky region, you can
switch from the torch to the climbing gear, but you cannot switch to neither.)

Finally, once you reach the target, you need the torch equipped before you can
find him in the dark. The target is always in a rocky region, so if you arrive
there with climbing gear equipped, you will need to spend seven minutes
switching to your torch.

For example, using the same cave system as above, starting in the top left
corner (0,0) and moving to the bottom right corner (the target, 10,10) as
quickly as possible, one possible route is as follows, with your current
position marked X:

Initially:

X=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down:
M=.|=.|.|=.|=|=.
X|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right:
M=.|=.|.|=.|=|=.
.X=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Switch from using the torch to neither tool:
M=.|=.|.|=.|=|=.
.X=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right 3:
M=.|=.|.|=.|=|=.
.|=|X|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Switch from using neither tool to the climbing gear:

M=.|=.|.|=.|=|=.
.|=|X|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down 7:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..X==..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..=X=..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down 3:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||.X.|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||..X|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Down:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.X..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Right 4:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=X||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Up 2:
M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===X===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

Switch from using the climbing gear to the torch:

M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===X===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||

This is tied with other routes as the fastest way to reach the target: 45 minutes. In it, 21
minutes are spent switching tools (three times, seven minutes each) and the remaining 24 minutes
are spent moving.

What is the fewest number of minutes you can take to reach the target?

*/

extern crate pathfinding;

use std::collections::HashMap;
use std::iter::FromIterator;

use pathfinding::directed::dijkstra::dijkstra_all;

type LengthUnit = usize;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct Coord {
  x: LengthUnit,
  y: LengthUnit,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct Boundary {
  max_x: LengthUnit,
  max_y: LengthUnit,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum TerrainType {
  Rocky,
  Wet,
  Narrow,
}

#[derive(Debug, Copy, Clone)]
struct Terrain {
  terrain_type: TerrainType,
  geological_index: usize,
  erosion_level: usize,
}

type Topology = HashMap<Coord, Terrain>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum EquipmentTool {
  None,
  ClimbingGear,
  Torch,
}

struct Map {
  topology: Topology,
  target: Coord,
  boundary: Boundary,
}

impl Map {
  fn new(depth: usize, target: &Coord, opt_boundary: Option<Boundary>) -> Self {
    let mut topology: Topology = HashMap::new();
    let boundary = if opt_boundary.is_some() {
      opt_boundary.unwrap()
    } else {
      Boundary {
        max_x: target.x,
        max_y: target.y,
      }
    };

    for y in 0..=boundary.max_y {
      for x in 0..=boundary.max_x {
        let coord = Coord { x, y };
        let geological_index = if coord == (Coord { x: 0, y: 0 }) || coord == *target {
          0
        } else if y == 0 {
          x * 16807
        } else if x == 0 {
          y * 48271
        } else {
          topology[&Coord { x: x - 1, y }].erosion_level
            * topology[&Coord { x, y: y - 1 }].erosion_level
        };
        let erosion_level = (depth + geological_index) % 20183;
        let terrain = Terrain {
          terrain_type: match erosion_level % 3 {
            0 => TerrainType::Rocky,
            1 => TerrainType::Wet,
            2 => TerrainType::Narrow,
            _ => TerrainType::Rocky,
          },
          erosion_level: erosion_level as usize,
          geological_index,
        };
        topology.insert(coord, terrain);
      }
    }

    Map {
      topology,
      target: *target,
      boundary,
    }
  }

  fn calc_risk_level(&self) -> usize {
    let mut total = 0;

    for y in 0..=self.target.y {
      for x in 0..=self.target.x {
        let coord = Coord { x, y };
        let delta = match self.topology[&coord].terrain_type {
          TerrainType::Rocky => 0,
          TerrainType::Wet => 1,
          TerrainType::Narrow => 2,
        };
        total += delta;
      }
    }

    total
  }

  fn get_print_text(&self) -> String {
    let mut lines: Vec<String> = vec![];

    for y in 0..=self.boundary.max_y {
      let mut line: Vec<char> = vec![];

      for x in 0..=self.boundary.max_x {
        let coord = Coord { x, y };
        let terrain = self.topology[&coord];
        let mut ch = match terrain.terrain_type {
          TerrainType::Rocky => '.',
          TerrainType::Wet => '=',
          TerrainType::Narrow => '|',
        };

        if coord == (Coord { x: 0, y: 0 }) {
          ch = 'M';
        } else if coord == self.target {
          ch = 'T';
        }

        line.push(ch);
      }

      lines.push(String::from_iter(line));
    }

    lines.join("\n")
  }

  #[allow(dead_code)]
  fn print(&self) {
    let text = self.get_print_text();

    for line in text.lines() {
      println!("{}", line);
    }
  }

  fn calc_least_minutes_to_target(&self) -> usize {
    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    struct FindingState {
      coord: Coord,
      equipment: EquipmentTool,
    };

    let init_finding_state = FindingState {
      coord: Coord { x: 0, y: 0 },
      equipment: EquipmentTool::Torch,
    };

    fn get_contiguous_coords(map: &Map, coord: &Coord) -> Vec<Coord> {
      let mut coords: Vec<Coord> = vec![];

      if coord.x > 0 {
        coords.push(Coord {
          x: coord.x - 1,
          y: coord.y,
        });
      }
      if coord.y > 0 {
        coords.push(Coord {
          x: coord.x,
          y: coord.y - 1,
        });
      }
      if coord.x < map.boundary.max_x {
        coords.push(Coord {
          x: coord.x + 1,
          y: coord.y,
        });
      }
      if coord.y < map.boundary.max_y {
        coords.push(Coord {
          x: coord.x,
          y: coord.y + 1,
        });
      }

      coords
    }

    fn get_terrain_of_coord(map: &Map, coord: &Coord) -> TerrainType {
      map.topology[coord].terrain_type
    }

    fn get_successors(map: &Map, finding_state: &FindingState) -> Vec<(FindingState, usize)> {
      let contiguous_coords = get_contiguous_coords(&map, &finding_state.coord);
      let current_terrain_type = get_terrain_of_coord(&map, &finding_state.coord);
      let mut options: Vec<(FindingState, usize)> = vec![];

      for contiguous_coord in contiguous_coords {
        let terrain_type = get_terrain_of_coord(&map, &contiguous_coord);

        if terrain_type != current_terrain_type {
          if terrain_type == TerrainType::Rocky && finding_state.equipment == EquipmentTool::None {
            options.push((
              FindingState {
                coord: contiguous_coord,
                equipment: if current_terrain_type == TerrainType::Narrow {
                  EquipmentTool::Torch
                } else {
                  EquipmentTool::ClimbingGear
                },
              },
              8,
            ));
            continue;
          }
          if terrain_type == TerrainType::Wet && finding_state.equipment == EquipmentTool::Torch {
            options.push((
              FindingState {
                coord: contiguous_coord,
                equipment: if current_terrain_type == TerrainType::Narrow {
                  EquipmentTool::None
                } else {
                  EquipmentTool::ClimbingGear
                },
              },
              8,
            ));
            continue;
          }
          if terrain_type == TerrainType::Narrow
            && finding_state.equipment == EquipmentTool::ClimbingGear
          {
            options.push((
              FindingState {
                coord: contiguous_coord,
                equipment: if current_terrain_type == TerrainType::Wet {
                  EquipmentTool::None
                } else {
                  EquipmentTool::Torch
                },
              },
              8,
            ));
            continue;
          }
        }

        options.push((
          FindingState {
            coord: contiguous_coord,
            equipment: finding_state.equipment,
          },
          1,
        ));
      }

      options
    }

    let dummy_finding = (
      FindingState {
        coord: Coord { x: 0, y: 0 },
        equipment: EquipmentTool::None,
      },
      100_000_000,
    );

    let all_results = dijkstra_all(&init_finding_state, |x| get_successors(&self, &x));
    let result_with_torch = all_results
      .get(&FindingState {
        coord: self.target,
        equipment: EquipmentTool::Torch,
      })
      .unwrap_or(&dummy_finding)
      .1;
    let result_with_none = all_results
      .get(&FindingState {
        coord: self.target,
        equipment: EquipmentTool::None,
      })
      .unwrap_or(&dummy_finding)
      .1
      + 7;
    let result_with_climbing = all_results
      .get(&FindingState {
        coord: self.target,
        equipment: EquipmentTool::ClimbingGear,
      })
      .unwrap_or(&dummy_finding)
      .1
      + 7;

    std::cmp::min(
      result_with_torch,
      std::cmp::min(result_with_climbing, result_with_none),
    )
  }
}

const INPUT_DEPTH: usize = 6969;
const INPUT_TARGET: Coord = Coord { x: 9, y: 796 };

fn main() {
  let map = Map::new(
    INPUT_DEPTH,
    &INPUT_TARGET,
    Some(Boundary {
      max_x: INPUT_TARGET.x + 100,
      max_y: INPUT_TARGET.y + 100,
    }),
  );
  let risk_level = map.calc_risk_level();
  let least_minutes = map.calc_least_minutes_to_target();

  println!("Results:");
  println!("- (1) rist level: {}", risk_level);
  println!("- (2) least minutes: {}", least_minutes);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_map() -> Map {
    Map::new(
      510,
      &Coord { x: 10, y: 10 },
      Some(Boundary {
        max_x: 15,
        max_y: 15,
      }),
    )
  }

  #[test]
  fn test_mapget_print_text() {
    let map = get_example_map();
    map.print();
    assert_eq!(
      map.get_print_text(),
      "M=.|=.|.|=.|=|=.
.|=|=|||..|.=...
.==|....||=..|==
=.|....|.==.|==.
=|..==...=.|==..
=||.=.=||=|=..|=
|.=.===|||..=..|
|..==||=.|==|===
.=..===..=|.|||.
.======|||=|=.|=
.===|=|===T===||
=|||...|==..|=.|
=.=|=.=..=.||==|
||=|=...|==.=|==
|=.=||===.|||===
||.|==.|.|.||=||"
    );
  }

  #[test]
  fn test_calc_risk_level() {
    let map = get_example_map();

    assert_eq!(map.calc_risk_level(), 114);
  }

  #[test]
  fn test_calc_least_minutes_to_target() {
    let map = get_example_map();
    let least_minutes = map.calc_least_minutes_to_target();

    assert_eq!(least_minutes, 45);
  }
}
