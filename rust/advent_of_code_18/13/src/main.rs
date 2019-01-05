/*

--- Day 13: Mine Cart Madness ---

A crop of this size requires significant logistics to transport produce, soil, fertilizer, and so
on. The Elves are very busy pushing things around in carts on some kind of rudimentary system of
tracks they've come up with.

Seeing as how cart-and-track systems don't appear in recorded history for another 1000 years, the
Elves seem to be making this up as they go along. They haven't even figured out how to avoid
collisions yet.

You map out the tracks (your puzzle input) and see where you can help.

Tracks consist of straight paths (| and -), curves (/ and \), and intersections (+). Curves connect
exactly two perpendicular pieces of track; for example, this is a closed loop:

/----\
|    |
|    |
\----/

Intersections occur when two perpendicular paths cross. At an intersection, a cart is capable of
turning left, turning right, or continuing straight. Here are two loops connected by two
intersections:

/-----\
|     |
|  /--+--\
|  |  |  |
\--+--/  |
   |     |
   \-----/

Several carts are also on the tracks. Carts always face either up (^), down (v), left (<), or right
(>). (On your initial map, the track under each cart is a straight path matching the direction the
cart is facing.)

Each time a cart has the option to turn (by arriving at any intersection), it turns left the first
time, goes straight the second time, turns right the third time, and then repeats those directions
starting again with left the fourth time, straight the fifth time, and so on. This process is
independent of the particular intersection at which the cart has arrived - that is, the cart has no
per-intersection memory.

Carts all move at the same speed; they take turns moving a single step at a time. They do this
based on their current location: carts on the top row move first (acting from left to right), then
carts on the second row move (again from left to right), then carts on the third row, and so on.
Once each cart has moved one step, the process repeats; each of these loops is called a tick.

For example, suppose there are two carts on a straight track:

|  |  |  |  |
v  |  |  |  |
|  v  v  |  |
|  |  |  v  X
|  |  ^  ^  |
^  ^  |  |  |
|  |  |  |  |

First, the top cart moves. It is facing down (v), so it moves down one square. Second, the bottom
cart moves. It is facing up (^), so it moves up one square. Because all carts have moved, the first
tick ends. Then, the process repeats, starting with the first cart. The first cart moves down, then
the second cart moves up - right into the first cart, colliding with it! (The location of the crash
is marked with an X.) This ends the second and last tick.

Here is a longer example:

/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/

/-->\
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \->--/
  \------/

/---v
|   |  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+>-/
  \------/

/---\
|   v  /----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-+->/
  \------/

/---\
|   |  /----\
| /->--+-\  |
| | |  | |  |
\-+-/  \-+--^
  \------/

/---\
|   |  /----\
| /-+>-+-\  |
| | |  | |  ^
\-+-/  \-+--/
  \------/

/---\
|   |  /----\
| /-+->+-\  ^
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /----<
| /-+-->-\  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /---<\
| /-+--+>\  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /--<-\
| /-+--+-v  |
| | |  | |  |
\-+-/  \-+--/
  \------/

/---\
|   |  /-<--\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/

/---\
|   |  /<---\
| /-+--+-\  |
| | |  | |  |
\-+-/  \-<--/
  \------/

/---\
|   |  v----\
| /-+--+-\  |
| | |  | |  |
\-+-/  \<+--/
  \------/

/---\
|   |  /----\
| /-+--v-\  |
| | |  | |  |
\-+-/  ^-+--/
  \------/

/---\
|   |  /----\
| /-+--+-\  |
| | |  X |  |
\-+-/  \-+--/
  \------/

After following their respective paths for a while, the carts eventually crash. To help prevent
crashes, you'd like to know the location of the first crash. Locations are given in X,Y
coordinates, where the furthest left column is X=0 and the furthest top row is Y=0:

           111
 0123456789012
0/---\
1|   |  /----\
2| /-+--+-\  |
3| | |  X |  |
4\-+-/  \-+--/
5  \------/

In this example, the location of the first crash is 7,3.

--- Part Two ---

There isn't much you can do to prevent crashes in this ridiculous system. However, by predicting
the crashes, the Elves know where to be in advance and instantly remove the two crashing carts the
moment any crash occurs.

They can proceed like this for a while, but eventually, they're going to run out of carts. It could
be useful to figure out where the last cart that hasn't crashed will end up.

For example:

/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/

/---\
|   |
| v-+-\
| | | |
\-+-/ |
  |   |
  ^---^

/---\
|   |
| /-+-\
| v | |
\-+-/ |
  ^   ^
  \---/

/---\
|   |
| /-+-\
| | | |
\-+-/ ^
  |   |
  \---/

After four very expensive crashes, a tick ends with only one cart remaining; its final location is
6,4.

What is the location of the last cart at the end of the first tick where it is the only cart left?

*/

use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

type Unit = i32;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct Coord {
  x: Unit,
  y: Unit,
}

#[derive(Debug, Eq, PartialEq)]
struct MapBoundaries {
  max_x: Unit,
  max_y: Unit,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Direction {
  Down,
  Up,
  Left,
  Right,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum IntersectionDirection {
  Left,
  Right,
  Straight,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct Car {
  coord: Coord,
  direction: Direction,
  last_instersection_direction: IntersectionDirection,
}

impl Ord for Car {
  fn cmp(&self, other: &Car) -> Ordering {
    match self.coord.y.cmp(&other.coord.y) {
      Ordering::Equal => self.coord.x.cmp(&other.coord.x),
      v => v,
    }
  }
}

impl PartialOrd for Car {
  fn partial_cmp(&self, other: &Car) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum RoadType {
  Vertical,
  Horizontal,
  TurnTopRight,
  TurnTopLeft,
  Intersection,
}

type RoadsMap = HashMap<Coord, Option<RoadType>>;
type Cars = Vec<Car>;
type TrafficState = (RoadsMap, Cars);

fn get_chars_vecs(a_str: String) -> Vec<Vec<char>> {
  let chars_vecs: Vec<Vec<char>> = a_str.lines().map(|x| x.chars().collect()).collect();

  chars_vecs
}

fn get_map_boundaries_from_chars_vecs(chars_vecs: &mut Vec<Vec<char>>) -> MapBoundaries {
  let mut max_x: Unit = 0;
  let mut max_y: Unit = 0;

  for (line_idx, line) in chars_vecs.iter().enumerate() {
    for (char_idx, _) in line.iter().enumerate() {
      if char_idx as Unit > max_x {
        max_x = char_idx as Unit;
      }
    }

    if line_idx as Unit > max_y {
      max_y = line_idx as Unit;
    }
  }

  MapBoundaries {
    max_x: max_x,
    max_y: max_y,
  }
}

fn parse_map_str(map_str: String) -> TrafficState {
  let mut chars_vecs = get_chars_vecs(map_str);
  let map_boundaries = get_map_boundaries_from_chars_vecs(&mut chars_vecs);
  let mut roads_map: RoadsMap = HashMap::new();
  let mut cars: Cars = vec![];

  for y in 0..=map_boundaries.max_y {
    let line = chars_vecs.get(y as usize).unwrap();

    for x in 0..=map_boundaries.max_x {
      let char_value = line.get(x as usize).unwrap_or(&' ');

      let coord = Coord { x: x, y: y };

      let map_value = match char_value {
        &'-' => Some(RoadType::Horizontal),
        &'|' => Some(RoadType::Vertical),
        &'>' => Some(RoadType::Horizontal),
        &'<' => Some(RoadType::Horizontal),
        &'v' => Some(RoadType::Vertical),
        &'^' => Some(RoadType::Vertical),
        &'+' => Some(RoadType::Intersection),
        &'\\' => Some(RoadType::TurnTopLeft),
        &'/' => Some(RoadType::TurnTopRight),
        _ => None,
      };

      roads_map.insert(coord, map_value);

      let mut car = Car {
        coord: coord,
        last_instersection_direction: IntersectionDirection::Right,
        direction: Direction::Right,
      };

      match char_value {
        &'>' => {
          cars.push(car);
        }
        &'<' => {
          car.direction = Direction::Left;
          cars.push(car);
        }
        &'v' => {
          car.direction = Direction::Down;
          cars.push(car);
        }
        &'^' => {
          car.direction = Direction::Up;
          cars.push(car);
        }
        _ => {}
      };
    }
  }

  (roads_map, cars)
}

fn get_direction_after_intersection(
  car_direction: Direction,
  intersection_direction: IntersectionDirection,
) -> Direction {
  match intersection_direction {
    IntersectionDirection::Left => match car_direction {
      Direction::Left => Direction::Down,
      Direction::Right => Direction::Up,
      Direction::Up => Direction::Left,
      Direction::Down => Direction::Right,
    },
    IntersectionDirection::Right => match car_direction {
      Direction::Left => Direction::Up,
      Direction::Right => Direction::Down,
      Direction::Up => Direction::Right,
      Direction::Down => Direction::Left,
    },
    IntersectionDirection::Straight => match car_direction {
      v => v,
    },
  }
}

fn calculate_first_crash_coord(
  roads_map: &mut RoadsMap,
  cars: &mut Cars,
  should_only_be_one: bool,
) -> Coord {
  let mut current_cars: Cars = cars.clone();

  loop {
    current_cars.sort();
    current_cars.reverse();

    let mut next_cars: Cars = vec![];
    let mut prev_car_coords: HashSet<Coord> = HashSet::new();
    let mut next_car_coords: HashSet<Coord> = HashSet::new();

    for car in current_cars.clone() {
      prev_car_coords.insert(car.coord);
    }

    loop {
      let car;

      match current_cars.pop() {
        Some(v) => {
          car = v;
        }
        None => {
          break;
        }
      };

      let next_car_coord = match car.direction {
        Direction::Left => Coord {
          x: car.coord.x - 1,
          y: car.coord.y,
        },
        Direction::Right => Coord {
          x: car.coord.x + 1,
          y: car.coord.y,
        },
        Direction::Up => Coord {
          x: car.coord.x,
          y: car.coord.y - 1,
        },
        Direction::Down => Coord {
          x: car.coord.x,
          y: car.coord.y + 1,
        },
      };
      let next_car_coord_road = roads_map.get(&next_car_coord).unwrap();
      let next_car_coord_unwrapped = next_car_coord_road.unwrap();
      let next_intersection_direction = if next_car_coord_unwrapped == RoadType::Intersection {
        match car.last_instersection_direction {
          IntersectionDirection::Right => IntersectionDirection::Left,
          IntersectionDirection::Left => IntersectionDirection::Straight,
          IntersectionDirection::Straight => IntersectionDirection::Right,
        }
      } else {
        car.last_instersection_direction
      };
      let next_car_direction = match next_car_coord_unwrapped {
        RoadType::Intersection => {
          get_direction_after_intersection(car.direction, next_intersection_direction)
        }
        RoadType::TurnTopLeft => match car.direction {
          Direction::Left => Direction::Up,
          Direction::Right => Direction::Down,
          Direction::Up => Direction::Left,
          Direction::Down => Direction::Right,
        },
        RoadType::TurnTopRight => match car.direction {
          Direction::Left => Direction::Down,
          Direction::Right => Direction::Up,
          Direction::Up => Direction::Right,
          Direction::Down => Direction::Left,
        },
        _ => car.direction,
      };

      let new_car = Car {
        coord: next_car_coord,
        last_instersection_direction: next_intersection_direction,
        direction: next_car_direction,
      };

      prev_car_coords.remove(&car.coord);

      let mut should_push_car = true;

      if next_car_coords.contains(&next_car_coord) {
        if should_only_be_one == false {
          return next_car_coord;
        }

        should_push_car = false;

        let index_to_remove = next_cars
          .iter()
          .position(|c| c.coord == next_car_coord)
          .unwrap();
        next_cars.remove(index_to_remove);
      } else if prev_car_coords.contains(&next_car_coord) {
        if should_only_be_one == false {
          return next_car_coord;
        }

        should_push_car = false;

        let index_to_remove = current_cars
          .iter()
          .position(|c| c.coord == next_car_coord)
          .unwrap();
        current_cars.remove(index_to_remove);
      }

      if should_push_car {
        next_car_coords.insert(next_car_coord);
        next_cars.push(new_car);
      }
    }

    if next_cars.iter().count() == 1 {
      return next_cars[0].coord;
    }

    current_cars = next_cars;
  }
}

fn get_input_str() -> String {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  contents
}

fn main() {
  let input_str = get_input_str();
  let (mut roads_map, mut cars) = parse_map_str(input_str);
  let coord = calculate_first_crash_coord(&mut roads_map, &mut cars, false);
  let coord_2 = calculate_first_crash_coord(&mut roads_map, &mut cars, true);

  println!("Results:");
  println!("- (1) coord of first crash: {:?}", coord);
  println!("- (2) coord of last car: {:?}", coord_2);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_map_str() -> String {
    "/->-\\
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/"
      .to_string()
  }

  fn get_example_2_map_str() -> String {
    "/>-<\\
|   |
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/"
      .to_string()
  }

  #[test]
  fn test_get_map_boundaries_from_map_str() {
    let map_str = "012
01234
01"
      .to_string();

    let mut chars_vecs_1 = get_chars_vecs(map_str);
    let map_boundaries = get_map_boundaries_from_chars_vecs(&mut chars_vecs_1);
    let example_map_str = get_example_map_str();
    let mut chars_vecs_2 = get_chars_vecs(example_map_str);
    let map_boundaries_2 = get_map_boundaries_from_chars_vecs(&mut chars_vecs_2);

    assert_eq!(map_boundaries, MapBoundaries { max_x: 4, max_y: 2 });
    assert_eq!(
      map_boundaries_2,
      MapBoundaries {
        max_x: 12,
        max_y: 5
      }
    );
  }

  #[test]
  fn test_parse_map_str() {
    let example_map_str = get_example_map_str();
    let (_, cars) = parse_map_str(example_map_str);

    assert_eq!(cars.iter().count(), 2);
  }

  #[test]
  fn test_calculate_first_crash_coord() {
    let example_map_str = get_example_map_str();
    let (mut roads_map, mut cars) = parse_map_str(example_map_str);
    let coord = calculate_first_crash_coord(&mut roads_map, &mut cars, false);

    assert_eq!(coord, Coord { x: 7, y: 3 });
  }

  #[test]
  fn test_second_exercise() {
    let example_map_str = get_example_2_map_str();
    let (mut roads_map, mut cars) = parse_map_str(example_map_str);
    let coord = calculate_first_crash_coord(&mut roads_map, &mut cars, true);

    assert_eq!(coord, Coord { x: 6, y: 4 });
  }

  #[test]
  fn test_cars_ordering() {
    let mut cars: Cars = vec![
      Car {
        coord: Coord { x: 2, y: 2 },
        last_instersection_direction: IntersectionDirection::Left,
        direction: Direction::Down,
      },
      Car {
        coord: Coord { x: 0, y: 0 },
        last_instersection_direction: IntersectionDirection::Left,
        direction: Direction::Down,
      },
      Car {
        coord: Coord { x: 0, y: 2 },
        last_instersection_direction: IntersectionDirection::Left,
        direction: Direction::Down,
      },
    ];

    cars.sort();

    let cars_coords: Vec<Coord> = cars.iter().map(|x| x.coord).collect();

    assert_eq!(
      cars_coords,
      vec![
        Coord { x: 0, y: 0 },
        Coord { x: 0, y: 2 },
        Coord { x: 2, y: 2 }
      ]
    );
  }
}
