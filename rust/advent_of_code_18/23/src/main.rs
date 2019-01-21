/*

--- Day 23: Experimental Emergency Teleportation ---

Using your torch to search the darkness of the rocky cavern, you finally locate the man's friend: a
small reindeer.

You're not sure how it got so far in this cave. It looks sick - too sick to walk - and too heavy
for you to carry all the way back. Sleighs won't be invented for another 1500 years, of course.

The only option is experimental emergency teleportation.

You hit the "experimental emergency teleportation" button on the device and push I accept the risk
on no fewer than 18 different warning messages. Immediately, the device deploys hundreds of tiny
nanobots which fly around the cavern, apparently assembling themselves into a very specific
formation. The device lists the X,Y,Z position (pos) for each nanobot as well as its signal radius
(r) on its tiny screen (your puzzle input).

Each nanobot can transmit signals to any integer coordinate which is a distance away from it less
than or equal to its signal radius (as measured by Manhattan distance). Coordinates a distance away
of less than or equal to a nanobot's signal radius are said to be in range of that nanobot.

Before you start the teleportation process, you should determine which nanobot is the strongest
(that is, which has the largest signal radius) and then, for that nanobot, the total number of
nanobots that are in range of it, including itself.

For example, given the following nanobots:

pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1

The strongest nanobot is the first one (position 0,0,0) because its signal radius, 4 is the
largest. Using that nanobot's location and signal radius, the following nanobots are in or out of
range:

The nanobot at 0,0,0 is distance 0 away, and so it is in range.
The nanobot at 1,0,0 is distance 1 away, and so it is in range.
The nanobot at 4,0,0 is distance 4 away, and so it is in range.
The nanobot at 0,2,0 is distance 2 away, and so it is in range.
The nanobot at 0,5,0 is distance 5 away, and so it is not in range.
The nanobot at 0,0,3 is distance 3 away, and so it is in range.
The nanobot at 1,1,1 is distance 3 away, and so it is in range.
The nanobot at 1,1,2 is distance 4 away, and so it is in range.
The nanobot at 1,3,1 is distance 5 away, and so it is not in range.

In this example, in total, 7 nanobots are in range of the nanobot with the largest signal radius.

Find the nanobot with the largest signal radius. How many nanobots are in range of its signals?

--- Part Two ---

Now, you just need to figure out where to position yourself so that you're actually teleported when
the nanobots activate.

To increase the probability of success, you need to find the coordinate which puts you in range of
the largest number of nanobots. If there are multiple, choose one closest to your position (0,0,0,
measured by manhattan distance).

For example, given the following nanobot formation:

pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5

Many coordinates are in range of some of the nanobots in this formation. However, only the
coordinate 12,12,12 is in range of the most nanobots: it is in range of the first five, but is not
in range of the nanobot at 10,10,10. (All other coordinates are in range of
fewer than five nanobots.) This coordinate's distance from 0,0,0 is 36.

Find the coordinates that are in range of the largest number of nanobots. What is the shortest
manhattan distance between any of those points and 0,0,0?

*/

extern crate regex;

use regex::Regex;
use std::fs::File;
use std::io::prelude::*;

type LengthUnit = i64;

#[derive(Debug, Clone, Copy)]
struct Coord {
  x: LengthUnit,
  y: LengthUnit,
  z: LengthUnit,
}

impl Coord {
  fn get_distance(&self, other_coord: &Coord) -> LengthUnit {
    (self.x - other_coord.x).abs() + (self.y - other_coord.y).abs() + (self.z - other_coord.z).abs()
  }
}

#[derive(Debug, Clone, Copy)]
struct Boundary {
  max_x: LengthUnit,
  max_y: LengthUnit,
  max_z: LengthUnit,
  min_x: LengthUnit,
  min_y: LengthUnit,
  min_z: LengthUnit,
}

#[derive(Debug, Clone, Copy)]
struct NanoBot {
  coord: Coord,
  signal_radius: LengthUnit,
  id: usize,
}

impl NanoBot {
  fn new_from_str(text: &str) -> Vec<Self> {
    let mut nanobots: Vec<Self> = vec![];
    let mut id = 0;

    let reg = Regex::new(r"pos=<(.+),(.+),(.+)>, r=(.+)$").unwrap();

    for line in text.lines() {
      let caps = reg.captures(line).unwrap();
      let nanobot = NanoBot {
        coord: Coord {
          x: caps.get(1).unwrap().as_str().parse::<LengthUnit>().unwrap(),
          y: caps.get(2).unwrap().as_str().parse::<LengthUnit>().unwrap(),
          z: caps.get(3).unwrap().as_str().parse::<LengthUnit>().unwrap(),
        },
        id,
        signal_radius: caps.get(4).unwrap().as_str().parse::<LengthUnit>().unwrap(),
      };
      nanobots.push(nanobot);
      id += 1;
    }

    nanobots
  }

  fn get_nanobots_num_in_range(nanobots: &Vec<NanoBot>) -> usize {
    let mut strongest_nanobot = nanobots[0];
    let mut num = 0;

    for nanobot in nanobots {
      if nanobot.signal_radius > strongest_nanobot.signal_radius {
        strongest_nanobot = *nanobot;
      }
    }

    for nanobot in nanobots {
      if nanobot.coord.get_distance(&strongest_nanobot.coord) <= strongest_nanobot.signal_radius {
        num += 1;
      }
    }

    num
  }

  fn get_boundary_of_nanobots(nanobots: &Vec<NanoBot>) -> Boundary {
    let mut boundary = Boundary {
      max_x: nanobots[0].coord.x,
      max_y: nanobots[0].coord.y,
      max_z: nanobots[0].coord.z,
      min_x: nanobots[0].coord.x,
      min_y: nanobots[0].coord.y,
      min_z: nanobots[0].coord.z,
    };

    for nanobot in nanobots {
      boundary.max_x = std::cmp::max(nanobot.coord.x, boundary.max_x);
      boundary.max_y = std::cmp::max(nanobot.coord.y, boundary.max_y);
      boundary.max_z = std::cmp::max(nanobot.coord.z, boundary.max_z);

      boundary.min_x = std::cmp::min(nanobot.coord.x, boundary.min_x);
      boundary.min_y = std::cmp::min(nanobot.coord.y, boundary.min_y);
      boundary.min_z = std::cmp::min(nanobot.coord.z, boundary.min_z);
    }

    boundary
  }

  fn get_best_min_distance(nanobots: &Vec<NanoBot>, orig_coord: &Coord) -> usize {
    let boundary = NanoBot::get_boundary_of_nanobots(&nanobots);

    println!("boundary {:?}", boundary);

    0
  }
}

fn get_input_nanobots() -> Vec<NanoBot> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  NanoBot::new_from_str(&contents)
}

fn main() {
  let input_nanobots = get_input_nanobots();
  let num_nanobots = NanoBot::get_nanobots_num_in_range(&input_nanobots);
  let best_min_distance =
    NanoBot::get_best_min_distance(&input_nanobots, &Coord { x: 0, y: 0, z: 0 });

  println!("Results:");
  println!("- (1) nanobots in range: {}", num_nanobots);
  println!("- (2) best min distance: {}", best_min_distance);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data() -> Vec<NanoBot> {
    NanoBot::new_from_str(
      "pos=<0,0,0>, r=4
    pos=<1,0,0>, r=1
    pos=<4,0,0>, r=3
    pos=<0,2,0>, r=1
    pos=<0,5,0>, r=3
    pos=<0,0,3>, r=1
    pos=<1,1,1>, r=1
    pos=<1,1,2>, r=1
    pos=<1,3,1>, r=1",
    )
  }

  fn get_example_data_2() -> Vec<NanoBot> {
    NanoBot::new_from_str(
      "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5",
    )
  }

  #[test]
  fn test_nanobot_new_from_str() {
    let nanobots = get_example_data();
    assert_eq!(nanobots[0].signal_radius, 4);
  }

  #[test]
  fn test_get_nanobots_num_in_range() {
    let nanobots = get_example_data();

    assert_eq!(NanoBot::get_nanobots_num_in_range(&nanobots), 7);
  }

  #[test]
  fn test_get_best_min_distance() {
    let nanobots = get_example_data_2();
    let best_min_distance = NanoBot::get_best_min_distance(&nanobots, &Coord { x: 0, y: 0, z: 0 });

    assert_eq!(best_min_distance, 36);
  }
}
