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

*/

extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

type LengthUnit = usize;

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

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum TerrainType {
  Sand,
  Clay,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Stream {
  parent: StreamId,
  children: Vec<StreamId>,
  id: StreamId,
  start: Coord,
  end: Coord,
}

type Streams = HashMap<StreamId, Stream>;

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
      min_y: 0,
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

    boundary.max_x += 1;
    boundary.min_x -= 1;

    boundary
  }
}

type MapTopology = HashMap<Coord, TerrainType>;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Map {
  topology: MapTopology,
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

fn main() {
  let clay_veins = get_input_clay_veins();
  let boundary = ClayVein::get_boundary_from_list(&clay_veins);

  println!("boundary {:?}", boundary);

  println!("Results");
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
        max_x: 507,
        min_x: 494,
        max_y: 13,
        min_y: 0
      }
    );
  }
}
