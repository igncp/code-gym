/*

--- Day 6: Chronal Coordinates ---

The device on your wrist beeps several times, and once again you feel like you're falling.

"Situation critical," the device announces. "Destination indeterminate. Chronal interference
detected. Please specify new target coordinates."

The device then produces a list of coordinates (your puzzle input). Are they places it thinks are
safe or dangerous? It recommends you check manual page 729. The Elves did not give you a manual.

If they're dangerous, maybe you can minimize the danger by finding the coordinate that gives the
largest distance from the other points.

Using only the Manhattan distance, determine the area around each coordinate by counting the number
of integer X,Y locations that are closest to that coordinate (and aren't tied in distance to any
other coordinate).

Your goal is to find the size of the largest area that isn't infinite. For example, consider the
following list of coordinates:

1, 1
1, 6
8, 3
3, 4
5, 5
8, 9

If we name these coordinates A through F, we can draw them on a grid, putting 0,0 at the top left:

..........
.A........
..........
........C.
...D......
.....E....
.B........
..........
..........
........F.

This view is partial - the actual grid extends infinitely in all directions. Using the Manhattan
distance, each location's closest coordinate can be determined, shown here in lowercase:

aaaaa.cccc
aAaaa.cccc
aaaddecccc
aadddeccCc
..dDdeeccc
bb.deEeecc
bBb.eeee..
bbb.eeefff
bbb.eeffff
bbb.ffffFf

Locations shown as . are equally far from two or more coordinates, and so they don't count as being
closest to any.

In this example, the areas of coordinates A, B, C, and F are infinite - while not shown here, their
areas extend forever outside the visible grid. However, the areas of coordinates D and E are
finite: D is closest to 9 locations, and E is closest to 17 (both including the coordinate's
location itself). Therefore, in this example, the size of the largest area is 17.

What is the size of the largest area that isn't infinite?

https://en.wikipedia.org/wiki/Taxicab_geometry

*/

extern crate regex;

use regex::Regex;
use std::fs::File;
use std::io::prelude::*;

type CoordUnit = i32;

#[derive(Debug, PartialEq, Eq)]
struct Coord {
  x: CoordUnit,
  y: CoordUnit,
}

impl Coord {
  fn new(x: CoordUnit, y: CoordUnit) -> Coord {
    Coord { x: x, y: y }
  }

  fn from_str(full_str: &str) -> Coord {
    let re = Regex::new(r"^(.*), (.*)$").unwrap();
    let caps = re.captures(full_str).unwrap();

    Coord {
      x: caps.get(1).unwrap().as_str().parse::<CoordUnit>().unwrap(),
      y: caps.get(2).unwrap().as_str().parse::<CoordUnit>().unwrap(),
    }
  }

  fn calc_manhattan_distance(&self, other: &Coord) -> usize {
    (self.x - other.x).abs() as usize + (self.y - other.y).abs() as usize
  }
}

fn get_input_coords() -> Vec<Coord> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  contents
    .lines()
    .map(|x| Coord::from_str(&x))
    .collect()
}

fn main() {
  let coords = get_input_coords();

  println!("Results:");
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_calc_manhattan_distance() {
    assert_eq!(
      Coord::new(0, 0).calc_manhattan_distance(&Coord::new(0, 0)),
      0
    );
    assert_eq!(
      Coord::new(2, 2).calc_manhattan_distance(&Coord::new(0, 0)),
      4
    );
    assert_eq!(
      Coord::new(-2, 2).calc_manhattan_distance(&Coord::new(0, 0)),
      4
    );
  }
}
