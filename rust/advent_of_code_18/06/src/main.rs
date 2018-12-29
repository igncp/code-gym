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

--- Part Two ---

On the other hand, if the coordinates are safe, maybe the best you can do is try to find a region
near as many coordinates as possible.

For example, suppose you want the sum of the Manhattan distance to all of the coordinates to be
less than 32. For each location, add up the distances to all of the given coordinates; if the total
of those distances is less than 32, that location is within the desired region. Using the same
coordinates as above, the resulting region looks like this:

..........
.A........
..........
...###..C.
..#D###...
..###E#...
.B.###....
..........
..........
........F.

In particular, consider the highlighted location 4,3 located at the top middle of the region. Its
calculation is as follows, where abs() is the absolute value function:

Distance to coordinate A: abs(4-1) + abs(3-1) =  5
Distance to coordinate B: abs(4-1) + abs(3-6) =  6
Distance to coordinate C: abs(4-8) + abs(3-3) =  4
Distance to coordinate D: abs(4-3) + abs(3-4) =  2
Distance to coordinate E: abs(4-5) + abs(3-5) =  3
Distance to coordinate F: abs(4-8) + abs(3-9) = 10
Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

Because the total distance to all coordinates (30) is less than 32, the location is within the
region.

This region, which also includes coordinates D and E, has a total size of 16.

Your actual region will need to be much larger than this example, though, instead including all
locations with a total distance of less than 10000.

What is the size of the region containing all locations which have a total distance to all given
coordinates of less than 10000?

*/

extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

type CoordUnit = i32;

#[derive(Debug, PartialEq, Eq)]
struct Boundary {
  max_x: CoordUnit,
  max_y: CoordUnit,
  min_x: CoordUnit,
  min_y: CoordUnit,
}

impl Boundary {
  fn is_point_on_edge(&self, point: &mut Coord) -> bool {
    if point.x == self.max_x || point.x == self.min_x {
      return true;
    }

    if point.y == self.max_y || point.y == self.min_y {
      return true;
    }

    return false;
  }
}

#[derive(Debug, PartialEq, Eq)]
struct Coord {
  x: CoordUnit,
  y: CoordUnit,
}

impl Coord {
  #[cfg(test)]
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

  contents.lines().map(|x| Coord::from_str(&x)).collect()
}

fn get_coords_boundary(coords: &mut Vec<Coord>) -> Boundary {
  let mut max_x = coords.get(0).unwrap().x;
  let mut min_x = coords.get(0).unwrap().x;
  let mut max_y = coords.get(0).unwrap().y;
  let mut min_y = coords.get(0).unwrap().y;

  for coord in coords {
    if max_x < coord.x {
      max_x = coord.x;
    }
    if max_y < coord.y {
      max_y = coord.y;
    }
    if min_x > coord.x {
      min_x = coord.x;
    }
    if min_y > coord.y {
      min_y = coord.y;
    }
  }

  Boundary {
    max_x: max_x,
    min_x: min_x,
    max_y: max_y,
    min_y: min_y,
  }
}

fn get_closest_vector_idx_if_one_for_point(
  coords: &mut Vec<Coord>,
  point: &mut Coord,
) -> Option<usize> {
  let mut current_distance = 0;
  let mut current_index = None;

  for (idx, coord) in coords.iter().enumerate() {
    let distance = point.calc_manhattan_distance(coord);

    if idx == 0 || distance < current_distance {
      current_distance = distance;
      current_index = Some(idx);
    } else if distance == current_distance {
      current_index = None;
    }
  }

  current_index
}

type AreaCount = usize;

fn get_biggest_finite_area(coords: &mut Vec<Coord>) -> Option<usize> {
  let boundary = get_coords_boundary(coords);
  type PointIdx = usize;
  let mut point_idx_to_count: HashMap<PointIdx, AreaCount> = HashMap::new();
  let mut impossible_index: HashSet<PointIdx> = HashSet::new();

  for x in boundary.min_x..=boundary.max_x {
    for y in boundary.min_y..=boundary.max_y {
      let mut point = Coord { x: x, y: y };
      let vec_idx = get_closest_vector_idx_if_one_for_point(coords, &mut point);

      match vec_idx {
        Some(v) => {
          let counter = point_idx_to_count.entry(v).or_insert(0);
          *counter += 1;

          if boundary.is_point_on_edge(&mut point) {
            impossible_index.insert(v);
          }
        }
        None => {}
      }
    }
  }

  let mut current_area: Option<usize> = None;

  for key in point_idx_to_count.keys() {
    let val = point_idx_to_count.get(key).unwrap();

    if impossible_index.contains(key) {
      continue;
    }

    if current_area.is_none() || current_area.unwrap() < *val {
      current_area = Some(*val);
    }
  }

  current_area
}

fn get_region_area_with_total_distance_smaller_than(
  coords: &mut Vec<Coord>,
  max_distance: usize,
) -> usize {
  let boundary = get_coords_boundary(coords);
  let mut area = 0;

  for x in boundary.min_x..=boundary.max_x {
    for y in boundary.min_y..=boundary.max_y {
      let point = Coord { x: x, y: y };
      let mut total_distance = 0;

      for (_, coord) in coords.iter_mut().enumerate() {
        let distance = point.calc_manhattan_distance(coord);
        total_distance += distance;

        if total_distance > max_distance {
          break;
        }
      }

      if total_distance < max_distance {
        area += 1;
      }
    }
  }

  area
}

const EXERCISE_MAX_DISTANCE: usize = 10000;

fn main() {
  let mut coords = get_input_coords();
  let biggest_finite_area = get_biggest_finite_area(&mut coords);
  let biggest_area_of_distance =
    get_region_area_with_total_distance_smaller_than(&mut coords, EXERCISE_MAX_DISTANCE);

  if biggest_finite_area.is_none() {
    panic!("Unexpected non-result");
  }

  println!("Results:");
  println!(
    "- (1) biggest finite area: {}",
    biggest_finite_area.unwrap()
  );
  println!(
    "- (2) biggest area with distance: {}",
    biggest_area_of_distance
  );
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_1_data() -> Vec<Coord> {
    vec![
      Coord::new(1, 1),
      Coord::new(1, 6),
      Coord::new(8, 3),
      Coord::new(3, 4),
      Coord::new(5, 5),
      Coord::new(8, 9),
    ]
  }

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

  #[test]
  fn test_get_coords_boundary() {
    let mut data = get_example_1_data();
    let result = get_coords_boundary(&mut data);

    assert_eq!(
      result,
      Boundary {
        max_x: 8,
        min_x: 1,
        max_y: 9,
        min_y: 1
      }
    );
  }

  #[test]
  fn test_get_closest_vector_idx_if_one_for_point() {
    fn test_this(point: Coord, expected_result: Option<usize>) {
      let mut point = point;
      let mut data = get_example_1_data();
      let result = get_closest_vector_idx_if_one_for_point(&mut data, &mut point);

      assert_eq!(result, expected_result);
    }

    test_this(Coord { x: 0, y: 0 }, Some(0));
    test_this(Coord { x: 8, y: 10 }, Some(5));
    test_this(Coord { x: 8, y: 9 }, Some(5));
  }

  #[test]
  fn test_get_biggest_finite_area() {
    let mut data = get_example_1_data();
    let result = get_biggest_finite_area(&mut data).unwrap();

    assert_eq!(result, 17);
  }

  #[test]
  fn test_get_region_area_with_total_distance_smaller_than() {
    let mut data = get_example_1_data();
    let result = get_region_area_with_total_distance_smaller_than(&mut data, 32);

    assert_eq!(result, 16);
  }
}
