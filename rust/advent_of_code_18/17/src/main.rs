/*

--- Day 17: Reservoir Research ---

You arrive in the year 18. If it weren't for the coat you got in 1018, you would be very cold: the
North Pole base hasn't even been constructed.

Rather, it hasn't been constructed yet. The Elves are making a little progress, but there's not a
lot of liquid water in this climate, so they're getting very dehydrated. Maybe there's more
underground?

You scan a two-dimensional vertical slice of the ground nearby and discover that it is mostly sand
with veins of clay. The scan only provides data with a granularity of square meters, but it should
be good enough to determine how much water is trapped there. In the scan, x represents the distance
to the right, and y represents the distance down. There is also a spring of water near the surface
at x=500, y=0. The scan identifies which square meters are clay (your puzzle input).

For example, suppose your scan shows the following veins of clay:

x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504

Rendering clay as #, sand as ., and the water spring as +, and with x increasing to the right and y
increasing downward, this becomes:

   44444455555555
   99999900000000
   45678901234567
 0 ......+.......
 1 ............#.
 2 .#..#.......#.
 3 .#..#..#......
 4 .#..#..#......
 5 .#.....#......
 6 .#.....#......
 7 .#######......
 8 ..............
 9 ..............
10 ....#.....#...
11 ....#.....#...
12 ....#.....#...
13 ....#######...

The spring of water will produce water forever. Water can move through sand, but is blocked by
clay. Water always moves down when possible, and spreads to the left and right otherwise, filling
space that has clay on both sides and falling out otherwise.

For example, if five squares of water are created, they will flow downward until they reach the
clay and settle there. Water that has come to rest is shown here as ~, while sand through which
water has passed (but which is now dry again) is shown as |:

......+.......
......|.....#.
.#..#.|.....#.
.#..#.|#......
.#..#.|#......
.#....|#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...

Two squares of water can't occupy the same location. If another five squares of water are created,
    they will settle on the first five, filling the clay reservoir a little more:

......+.......
......|.....#.
.#..#.|.....#.
.#..#.|#......
.#..#.|#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...

Water pressure does not apply in this scenario. If another four squares of water are created, they
will stay on the right side of the barrier, and no water will reach the left side:

......+.......
......|.....#.
.#..#.|.....#.
.#..#~~#......
.#..#~~#......
.#~~~~~#......
.#~~~~~#......
.#######......
..............
..............
....#.....#...
....#.....#...
....#.....#...
....#######...

At this point, the top reservoir overflows. While water can reach the tiles above the surface of
the water, it cannot settle there, and so the next five squares of water settle like this:

......+.......
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
........|.....
....#...|.#...
....#...|.#...
....#~~~~~#...
....#######...

Note especially the leftmost |: the new squares of water can reach this tile, but cannot stop
there. Instead, eventually, they all fall to the right and settle in the reservoir below.

After 10 more squares of water, the bottom reservoir is also full:

......+.......
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
........|.....
....#~~~~~#...
....#~~~~~#...
....#~~~~~#...
....#######...

Finally, while there is nowhere left for the water to settle, it can reach a few more tiles before
overflowing beyond the bottom of the scanned data:

......+.......    (line not counted: above minimum y value)
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
...|||||||||..
...|#~~~~~#|..
...|#~~~~~#|..
...|#~~~~~#|..
...|#######|..
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)

How many tiles can be reached by the water? To prevent counting forever, ignore tiles with a y
coordinate smaller than the smallest y coordinate in your scan data or larger than the largest one.
Any x coordinate is valid. In this example, the lowest y coordinate given is 1, and the highest is
13, causing the water spring (in row 0) and the water falling off the bottom of the render (in rows
                                                                                            14
                                                                                            through
                                                                                            infinity)
to be ignored.

So, in the example above, counting both water at rest (~) and other sand tiles the water can
hypothetically reach (|), the total number of tiles the water can reach is 57.

How many tiles can the water reach within the range of y values in your scan?

--- Part Two ---

After a very long time, the water spring will run dry. How much water will be retained?

In the example above, water that won't eventually drain out is shown as ~, a total of 29 tiles.

How many water tiles are left after the water spring stops producing water and all remaining water
not at rest has drained?

*/

extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

type LengthUnit = usize;

const ORIG_STREAM_SOURCE: Coord = Coord { x: 500, y: 0 };

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct Coord {
  x: LengthUnit,
  y: LengthUnit,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct Boundary {
  max_x: LengthUnit,
  max_y: LengthUnit,
  min_x: LengthUnit,
  min_y: LengthUnit,
}

type StreamId = usize;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Stream {
  id: StreamId,
  start: Coord,
  end: Coord,
}

impl Stream {
  fn get_coords_with_stream(streams: &StreamsCollection) -> HashSet<Coord> {
    let mut coords: HashSet<Coord> = HashSet::new();

    for (_, stream) in streams.iter() {
      for y in stream.start.y..=stream.end.y {
        coords.insert(Coord {
          y,
          x: stream.start.x,
        });
      }
    }

    coords
  }
}

type StreamsCollection = HashMap<StreamId, Stream>;

#[derive(Debug, Eq, PartialEq, Clone)]
struct ClayVein {
  from: Coord,
  to: Coord,
}

impl ClayVein {
  fn from_line(line: &str, regex: &(Regex, Regex)) -> Self {
    fn parse_match(v: Option<regex::Match>) -> LengthUnit {
      v.unwrap().as_str().parse::<LengthUnit>().unwrap()
    }

    fn create_vein(x1: LengthUnit, y1: LengthUnit, x2: LengthUnit, y2: LengthUnit) -> ClayVein {
      let coords = vec![Coord { x: x1, y: y1 }, Coord { x: x2, y: y2 }];

      ClayVein {
        from: coords[0],
        to: coords[1],
      }
    }

    let caps_x_y = regex.0.captures(&line);

    if caps_x_y.is_some() {
      let caps_x_y = caps_x_y.unwrap();
      let v1 = parse_match(caps_x_y.get(1));
      let v2 = parse_match(caps_x_y.get(2));
      let v3 = parse_match(caps_x_y.get(3));

      return create_vein(v1, v2, v1, v3);
    }

    let caps_y_x = regex.1.captures(&line);
    let caps_y_x = caps_y_x.unwrap();
    let v1 = parse_match(caps_y_x.get(1));
    let v2 = parse_match(caps_y_x.get(2));
    let v3 = parse_match(caps_y_x.get(3));

    create_vein(v2, v1, v3, v1)
  }

  fn get_line_regex() -> (Regex, Regex) {
    (
      Regex::new(r"^x=(\d+), y=(\d+)\.\.(\d+)$").unwrap(),
      Regex::new(r"^y=(\d+), x=(\d+)\.\.(\d+)$").unwrap(),
    )
  }

  fn get_boundary_from_list(list: &[ClayVein]) -> Boundary {
    let mut boundary = Boundary {
      max_x: list[0].from.x,
      min_y: list[0].from.y,
      min_x: list[0].from.x,
      max_y: list[0].from.y,
    };

    for item in list {
      if item.from.x < boundary.min_x {
        boundary.min_x = item.from.x;
      }
      if item.from.y < boundary.min_y {
        boundary.min_y = item.from.y;
      }
      if item.to.x > boundary.max_x {
        boundary.max_x = item.to.x;
      }
      if item.to.y > boundary.max_y {
        boundary.max_y = item.to.y;
      }
    }

    boundary
  }

  fn get_coords_with_clay(clay_veins: &[ClayVein]) -> HashSet<Coord> {
    let mut coords: HashSet<Coord> = HashSet::new();

    for clay_vein in clay_veins {
      if clay_vein.from.x == clay_vein.to.x {
        for y in clay_vein.from.y..=clay_vein.to.y {
          coords.insert(Coord {
            x: clay_vein.from.x,
            y,
          });
        }
      } else if clay_vein.from.y == clay_vein.to.y {
        for x in clay_vein.from.x..=clay_vein.to.x {
          coords.insert(Coord {
            x,
            y: clay_vein.from.y,
          });
        }
      }
    }

    coords
  }
}

fn get_input_clay_veins() -> Vec<ClayVein> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let mut veins: Vec<ClayVein> = vec![];
  let reg = ClayVein::get_line_regex();

  for line in contents.lines() {
    let result = ClayVein::from_line(line, &reg);

    veins.push(result);
  }

  veins
}

#[allow(dead_code)]
fn print_current_state(
  clay_veins: &[ClayVein],
  streams: &StreamsCollection,
  reservoir_coords: &HashSet<Coord>,
  flowing_water_coords: &HashSet<Coord>,
) {
  let boundary = ClayVein::get_boundary_from_list(&clay_veins);
  let coords_with_clay = ClayVein::get_coords_with_clay(&clay_veins);
  let coords_with_stream = Stream::get_coords_with_stream(&streams);

  println!();

  for y in boundary.min_y..=(boundary.max_y + 1) {
    for x in (boundary.min_x - 1)..=(boundary.max_x + 1) {
      let coord = Coord { x, y };

      if flowing_water_coords.contains(&coord) {
        print!("|");
      } else if reservoir_coords.contains(&coord) {
        print!("~");
      } else if coords_with_stream.contains(&coord) {
        print!("|");
      } else if coords_with_clay.contains(&coord) {
        print!("#");
      } else if coord == ORIG_STREAM_SOURCE {
        print!("+");
      } else {
        print!(".");
      }
    }

    println!();
  }

  println!();
}

fn count_tiles_with_stream(clay_veins: &[ClayVein]) -> (usize, usize) {
  let boundary = ClayVein::get_boundary_from_list(&clay_veins);
  let coords_with_clay = ClayVein::get_coords_with_clay(&clay_veins);
  let first_stream_start = Coord {
    x: ORIG_STREAM_SOURCE.x,
    y: ORIG_STREAM_SOURCE.y + 1,
  };

  let mut streams: StreamsCollection = HashMap::new();
  let mut active_streams_ids: HashSet<StreamId> = HashSet::new();
  let mut reservoir_coords: HashSet<Coord> = HashSet::new();
  let mut flowing_water_coords: HashSet<Coord> = HashSet::new();
  let mut started_streams: HashSet<Coord> = HashSet::new();

  let mut current_id = 0;

  let first_stream = Stream {
    id: 0,
    end: first_stream_start,
    start: first_stream_start,
  };

  streams.insert(0, first_stream);
  active_streams_ids.insert(0);

  while !active_streams_ids.is_empty() {
    let mut inactive_ids: Vec<StreamId> = vec![];
    let mut new_active_ids: Vec<StreamId> = vec![];

    for active_stream_id in active_streams_ids.iter() {
      streams.get_mut(active_stream_id).unwrap().end.y += 1;

      let end_coord = streams.get_mut(active_stream_id).unwrap().end;
      let next_coord = Coord {
        x: end_coord.x,
        y: end_coord.y + 1,
      };

      fn create_new_stream(
        current_id: &mut usize,
        coord: &Coord,
        streams: &mut StreamsCollection,
        new_active_ids: &mut Vec<StreamId>,
        started_streams: &mut HashSet<Coord>,
      ) {
        if started_streams.contains(&coord) {
          return;
        }

        *current_id += 1;
        let current_id = *current_id;
        let new_stream = Stream {
          start: *coord,
          end: *coord,
          id: current_id,
        };
        streams.insert(current_id, new_stream);
        new_active_ids.push(current_id);
        started_streams.insert(*coord);
      }

      if coords_with_clay.contains(&next_coord) {
        let mut has_two_clay_limits = true;

        streams.get_mut(active_stream_id).unwrap().end.y += 1;

        while has_two_clay_limits {
          streams.get_mut(active_stream_id).unwrap().end.y -= 1;
          let end_coord = streams.get_mut(active_stream_id).unwrap().end;

          let mut coord_at_right = end_coord;
          let mut coord_at_left = end_coord;

          let mut tmp_water_coords: HashSet<Coord> = HashSet::new();
          tmp_water_coords.insert(end_coord);

          loop {
            coord_at_right.x += 1;
            let coord_below_right = Coord {
              x: coord_at_right.x,
              y: coord_at_right.y + 1,
            };

            if !coords_with_clay.contains(&coord_below_right)
              && !reservoir_coords.contains(&coord_below_right)
            {
              has_two_clay_limits = false;
              create_new_stream(
                &mut current_id,
                &coord_at_right,
                &mut streams,
                &mut new_active_ids,
                &mut started_streams,
              );
              break;
            } else if coords_with_clay.contains(&coord_at_right) {
              break;
            }

            tmp_water_coords.insert(coord_at_right);
          }

          loop {
            coord_at_left.x -= 1;
            let coord_below_left = Coord {
              x: coord_at_left.x,
              y: coord_at_left.y + 1,
            };

            if !coords_with_clay.contains(&coord_below_left)
              && !reservoir_coords.contains(&coord_below_left)
            {
              has_two_clay_limits = false;
              create_new_stream(
                &mut current_id,
                &coord_at_left,
                &mut streams,
                &mut new_active_ids,
                &mut started_streams,
              );
              break;
            } else if coords_with_clay.contains(&coord_at_left) {
              break;
            }

            tmp_water_coords.insert(coord_at_left);
          }

          if has_two_clay_limits {
            for tmp_water_coord in tmp_water_coords {
              reservoir_coords.insert(tmp_water_coord);
            }
          } else {
            for tmp_water_coord in tmp_water_coords {
              flowing_water_coords.insert(tmp_water_coord);
            }
          }
        }

        inactive_ids.push(*active_stream_id)
      } else if next_coord.y == boundary.max_y + 1 {
        inactive_ids.push(*active_stream_id);
        break;
      }
    }

    for id in inactive_ids {
      active_streams_ids.remove(&id);
    }

    for id in new_active_ids {
      active_streams_ids.insert(id);
    }
  }

  let coords_with_stream = Stream::get_coords_with_stream(&streams);

  let mut all_water_set: HashSet<Coord> = HashSet::new();

  fn add_coords_of_set(set: &HashSet<Coord>, boundary: &Boundary, final_set: &mut HashSet<Coord>) {
    for coord in set {
      if coord.y >= boundary.min_y {
        final_set.insert(*coord);
      }
    }
  }

  add_coords_of_set(&coords_with_stream, &boundary, &mut all_water_set);
  add_coords_of_set(&flowing_water_coords, &boundary, &mut all_water_set);
  add_coords_of_set(&reservoir_coords, &boundary, &mut all_water_set);

  (all_water_set.len(), reservoir_coords.len())
}

fn main() {
  let clay_veins = get_input_clay_veins();
  let (count_all, count_remaining) = count_tiles_with_stream(&clay_veins);

  println!("Results");
  println!("- (1) count: {:?}", count_all);
  println!("- (2) count: {:?}", count_remaining);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data() -> Vec<ClayVein> {
    let reg = ClayVein::get_line_regex();

    vec![
      "x=495, y=2..7",
      "y=7, x=495..501",
      "x=501, y=3..7",
      "x=498, y=2..4",
      "x=506, y=1..2",
      "x=498, y=10..13",
      "x=504, y=10..13",
      "y=13, x=498..504",
    ]
    .iter()
    .map(|x| ClayVein::from_line(x, &reg))
    .collect()
  }

  #[test]
  fn test_clay_vein_from_line() {
    let reg = ClayVein::get_line_regex();

    assert_eq!(
      ClayVein::from_line("x=495, y=2..7", &reg),
      ClayVein {
        from: Coord { x: 495, y: 2 },
        to: Coord { x: 495, y: 7 }
      }
    );
    assert_eq!(
      ClayVein::from_line("y=7, x=495..501", &reg),
      ClayVein {
        from: Coord { x: 495, y: 7 },
        to: Coord { x: 501, y: 7 }
      }
    );
  }

  #[test]
  fn test_get_boundary_from_list() {
    let list = get_example_data();
    let boundary = ClayVein::get_boundary_from_list(&list);

    assert_eq!(
      boundary,
      Boundary {
        max_x: 506,
        min_x: 495,
        max_y: 13,
        min_y: 1
      }
    );
  }

  #[test]
  fn test_count_tiles_with_stream() {
    let list = get_example_data();
    let (result_all, result_remain) = count_tiles_with_stream(&list);

    assert_eq!(result_all, 57);
    assert_eq!(result_remain, 29);
  }
}
