/*

--- Day 10: The Stars Align ---

It's no use; your navigation system simply isn't capable of providing walking directions in the
arctic circle, and certainly not in 1018.

The Elves suggest an alternative. In times like these, North Pole rescue operations will arrange
points of light in the sky to guide missing Elves back to base. Unfortunately, the message is easy
to miss: the points move slowly enough that it takes hours to align them, but have so much momentum
that they only stay aligned for a second. If you blink at the wrong time, it might be hours before
another message appears.

You can see these points of light floating in the distance, and record their position in the sky
and their velocity, the relative change in position per second (your puzzle input). The coordinates
are all given from your perspective; given enough time, those positions and velocities will move
the points into a cohesive message!

Rather than wait, you decide to fast-forward the process and calculate what the points will
eventually spell.

For example, suppose you note the following points:

position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>

Each line represents one point. Positions are given as <X, Y> pairs: X represents how far left
(negative) or right (positive) the point appears, while Y represents how far up (negative) or down
(positive) the point appears.

At 0 seconds, each point has the position given. Each second, each point's velocity is added to its
position. So, a point with velocity <1, -2> is moving to the right, but is moving upward twice as
quickly. If this point's initial position were <3, 9>, after 3 seconds, its position would become
<6, 3>.

Over time, the points listed above would move like this:

Initially:
........#.............
................#.....
.........#.#..#.......
......................
#..........#.#.......#
...............#......
....#.................
..#.#....#............
.......#..............
......#...............
...#...#.#...#........
....#..#..#.........#.
.......#..............
...........#..#.......
#...........#.........
...#.......#..........

After 1 second:
......................
......................
..........#....#......
........#.....#.......
..#.........#......#..
......................
......#...............
....##.........#......
......#.#.............
.....##.##..#.........
........#.#...........
........#...#.....#...
..#...........#.......
....#.....#.#.........
......................
......................

After 2 seconds:
......................
......................
......................
..............#.......
....#..#...####..#....
......................
........#....#........
......#.#.............
.......#...#..........
.......#..#..#.#......
....#....#.#..........
.....#...#...##.#.....
........#.............
......................
......................
......................

After 3 seconds:
......................
......................
......................
......................
......#...#..###......
......#...#...#.......
......#...#...#.......
......#####...#.......
......#...#...#.......
......#...#...#.......
......#...#...#.......
......#...#..###......
......................
......................
......................
......................

After 4 seconds:
......................
......................
......................
............#.........
........##...#.#......
......#.....#..#......
.....#..##.##.#.......
.......##.#....#......
...........#....#.....
..............#.......
....#......#...#......
.....#.....##.........
...............#......
...............#......
......................
......................

After 3 seconds, the message appeared briefly: HI. Of course, your message will be much longer and
will take many more seconds to appear.

What message will eventually appear in the sky?

*/

use regex::Regex;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

type LengthUnit = f64;
type SpeedUnit = f64;

#[derive(Debug, Clone)]
struct Coord {
  x: LengthUnit,
  y: LengthUnit,
}

#[derive(Debug, Clone)]
struct Boundary {
  max_x: LengthUnit,
  min_x: LengthUnit,
  max_y: LengthUnit,
  min_y: LengthUnit,
}

#[derive(Debug, Clone)]
struct Velocity {
  x: SpeedUnit,
  y: SpeedUnit,
}

#[derive(Debug, Clone)]
struct Point {
  position: Coord,
  velocity: Velocity,
}

impl Point {
  fn create_from_descriptions(descriptions: &mut Vec<&str>) -> Vec<Point> {
    let reg = Regex::new("^position=<[ ]*(.+),[ ]*(.+)> velocity=<[ ]*(.+),[ ]*(.+)>$").unwrap();
    let mut points = vec![];

    for description in descriptions {
      let caps = reg.captures(description).unwrap();
      let point = Point {
        position: Coord {
          x: caps.get(1).unwrap().as_str().parse::<LengthUnit>().unwrap(),
          y: caps.get(2).unwrap().as_str().parse::<LengthUnit>().unwrap(),
        },
        velocity: Velocity {
          x: caps.get(3).unwrap().as_str().parse::<SpeedUnit>().unwrap(),
          y: caps.get(4).unwrap().as_str().parse::<SpeedUnit>().unwrap(),
        },
      };
      points.push(point);
    }

    points
  }

  fn calculate_second_of_min_boundary(
    points: &mut Vec<Point>,
    seconds_beginning: usize,
    seconds_end: usize,
  ) -> usize {
    let points = points.clone();
    let mut min_boundary_dimensions: Option<LengthUnit> = None;
    let mut chosen_second = 0;
    let mut points = points;

    points.iter_mut().for_each(|point| {
      point.move_time_n_seconds_for_point(seconds_beginning);
    });

    for second in seconds_beginning..seconds_end {
      let mut boundary = Point::calculate_boundary_of_points(&mut points);
      let boundary_dimensions = Point::calculate_boundary_dimensions(&mut boundary);

      if min_boundary_dimensions.is_none() || boundary_dimensions < min_boundary_dimensions.unwrap()
      {
        min_boundary_dimensions = Some(boundary_dimensions);
        chosen_second = second;
      }

      points.iter_mut().for_each(|point| {
        point.move_time_n_seconds_for_point(1);
      });
    }

    chosen_second
  }

  fn calculate_boundary_of_points(points: &mut Vec<Point>) -> Boundary {
    let mut boundary = Boundary {
      min_x: points[0].position.x,
      max_x: points[0].position.x,
      min_y: points[0].position.y,
      max_y: points[0].position.y,
    };

    points.iter_mut().for_each(|point| {
      if point.position.x < boundary.min_x {
        boundary.min_x = point.position.x;
      }
      if point.position.x > boundary.max_x {
        boundary.max_x = point.position.x;
      }
      if point.position.y < boundary.min_y {
        boundary.min_y = point.position.y;
      }
      if point.position.y > boundary.max_y {
        boundary.max_y = point.position.y;
      }
    });

    boundary
  }

  fn calculate_boundary_dimensions(boundary: &mut Boundary) -> LengthUnit {
    (boundary.max_x - boundary.min_x) + (boundary.max_y - boundary.min_y)
  }

  fn move_time_n_seconds_for_point(&mut self, seconds: usize) {
    self.position.x += &self.velocity.x * (seconds as SpeedUnit);
    self.position.y += &self.velocity.y * (seconds as SpeedUnit);
  }

  fn write_file_for_seconds(points: &mut Vec<Point>, second: usize) {
    use std::iter::FromIterator;

    let mut points = points.clone();

    for point in points.iter_mut() {
      point.move_time_n_seconds_for_point(second);
    }

    let boundary = Point::calculate_boundary_of_points(&mut points);
    let mut contents: Vec<Vec<char>> = vec![];

    for _ in 0..=(boundary.max_y as i32 - boundary.min_y as i32) {
      let mut line = vec![];
      for _ in 0..=(boundary.max_x as i32 - boundary.min_x as i32) {
        line.push('_');
      }
      contents.push(line);
    }

    for point in points.iter_mut() {
      let new_coord = Coord {
        x: point.position.x - boundary.min_x,
        y: point.position.y - boundary.min_y,
      };

      contents[new_coord.y as usize][new_coord.x as usize] = 'x';
    }

    let path = Path::new("result.txt");

    let mut file = File::create(&path).unwrap();
    file
      .write_all(
        contents
          .iter()
          .map(|x| String::from_iter(x))
          .collect::<Vec<String>>()
          .join("\n")
          .as_bytes(),
      )
      .unwrap();
  }
}

fn get_input_points() -> Vec<Point> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let mut descriptions: Vec<&str> = contents.lines().map(|x| x).collect();

  Point::create_from_descriptions(&mut descriptions)
}

fn main() {
  let mut input_points = get_input_points();

  let second = Point::calculate_second_of_min_boundary(&mut input_points, 0, 20000);

  Point::write_file_for_seconds(&mut input_points, second);

  println!("Results");
  println!("- (1) wrote file with second: {}", second);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_points() -> Vec<Point> {
    let mut descriptions = vec![
      "position=< 9,  1> velocity=< 0,  2>",
      "position=< 7,  0> velocity=<-1,  0>",
      "position=< 3, -2> velocity=<-1,  1>",
      "position=< 6, 10> velocity=<-2, -1>",
      "position=< 2, -4> velocity=< 2,  2>",
      "position=<-6, 10> velocity=< 2, -2>",
      "position=< 1,  8> velocity=< 1, -1>",
      "position=< 1,  7> velocity=< 1,  0>",
      "position=<-3, 11> velocity=< 1, -2>",
      "position=< 7,  6> velocity=<-1, -1>",
      "position=<-2,  3> velocity=< 1,  0>",
      "position=<-4,  3> velocity=< 2,  0>",
      "position=<10, -3> velocity=<-1,  1>",
      "position=< 5, 11> velocity=< 1, -2>",
      "position=< 4,  7> velocity=< 0, -1>",
      "position=< 8, -2> velocity=< 0,  1>",
      "position=<15,  0> velocity=<-2,  0>",
      "position=< 1,  6> velocity=< 1,  0>",
      "position=< 8,  9> velocity=< 0, -1>",
      "position=< 3,  3> velocity=<-1,  1>",
      "position=< 0,  5> velocity=< 0, -1>",
      "position=<-2,  2> velocity=< 2,  0>",
      "position=< 5, -2> velocity=< 1,  2>",
      "position=< 1,  4> velocity=< 2,  1>",
      "position=<-2,  7> velocity=< 2, -2>",
      "position=< 3,  6> velocity=<-1, -1>",
      "position=< 5,  0> velocity=< 1,  0>",
      "position=<-6,  0> velocity=< 2,  0>",
      "position=< 5,  9> velocity=< 1, -2>",
      "position=<14,  7> velocity=<-2,  0>",
      "position=<-3,  6> velocity=< 2, -1>",
    ];

    Point::create_from_descriptions(&mut descriptions)
  }

  #[test]
  fn test_calculate_second_of_min_boundary() {
    let mut points = get_example_points();
    let second = Point::calculate_second_of_min_boundary(&mut points, 0, 10);

    assert_eq!(second, 3);
  }
}
