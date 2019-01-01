/*

 --- Day 11: Chronal Charge ---

You watch the Elves and their sleigh fade into the distance as they head toward the North Pole.

Actually, you're the one fading. The falling sensation returns.

The low fuel warning light is illuminated on your wrist-mounted device. Tapping it once causes it
to project a hologram of the situation: a 300x300 grid of fuel cells and their current power
levels, some negative. You're not sure what negative power means in the context of time travel, but
it can't be good.

Each fuel cell has a coordinate ranging from 1 to 300 in both the X (horizontal) and Y (vertical)
direction. In X,Y notation, the top-left cell is 1,1, and the top-right cell is 300,1.

The interface lets you select any 3x3 square of fuel cells. To increase your chances of getting to
your destination, you decide to choose the 3x3 square with the largest total power.

The power level in a given fuel cell can be found through the following process:

Find the fuel cell's rack ID, which is its X coordinate plus 10.
Begin with a power level of the rack ID times the Y coordinate.
Increase the power level by the value of the grid serial number (your puzzle input).
Set the power level to itself multiplied by the rack ID.
Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
Subtract 5 from the power level.
For example, to find the power level of the fuel cell at 3,5 in a grid with serial number 8:

The rack ID is 3 + 10 = 13.
The power level starts at 13 * 5 = 65.
Adding the serial number produces 65 + 8 = 73.
Multiplying by the rack ID produces 73 * 13 = 949.
The hundreds digit of 949 is 9.
Subtracting 5 produces 9 - 5 = 4.
So, the power level of this fuel cell is 4.

Here are some more example power levels:

Fuel cell at  122,79, grid serial number 57: power level -5.
Fuel cell at 217,196, grid serial number 39: power level  0.
Fuel cell at 101,153, grid serial number 71: power level  4.

Your goal is to find the 3x3 square which has the largest total power. The square must be entirely
within the 300x300 grid. Identify this square using the X,Y coordinate of its top-left fuel cell.
For example:

For grid serial number 18, the largest total 3x3 square has a top-left corner of 33,45 (with a
total power of 29); these fuel cells appear in the middle of this 5x5 region:

-2  -4   4   4   4
-4   4   4   4  -5
 4   3   3   4  -4
 1   1   2   4  -3
-1   0   2  -5  -2

For grid serial number 42, the largest 3x3 square's top-left is 21,61 (with a total power of 30);
they are in the middle of this region:

-3   4   2   2   2
-4   4   3   3   4
-5   3   3   4  -4
 4   3   3   4  -3
 3   3   3  -5  -1

What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?

Your puzzle input is 2568.

--- Part Two ---

You discover a dial on the side of the device; it seems to let you select a square of any size, not
just 3x3. Sizes from 1x1 to 300x300 are supported.

Realizing this, you now must find the square of any size with the largest total power. Identify
this square by including its size as a third parameter after the top-left coordinate: a 9x9 square
with a top-left corner of 3,5 is identified as 3,5,9.

For example:

For grid serial number 18, the largest total square (with a total power of 113) is 16x16 and has a
top-left corner of 90,269, so its identifier is 90,269,16.

For grid serial number 42, the largest total square (with a total power of 119) is 12x12 and has a
top-left corner of 232,251, so its identifier is 232,251,12.

What is the X,Y,size identifier of the square with the largest total power?

*/

use std::cmp::{max, min};

type Unit = i64;
type SerialNumber = Unit;

const GRID_SERIAL_NUMBER: SerialNumber = 2568;

#[derive(Debug, Clone, PartialEq)]
struct Coord {
  x: Unit,
  y: Unit,
}

fn calc_power_level_for_coord(coord: &mut Coord, serial_number: Unit) -> Unit {
  let rack_id = coord.x + 10;
  let mut power_level = rack_id * coord.y;

  power_level += serial_number;
  power_level = power_level * rack_id;

  let hundreds_digit = match power_level >= 100 {
    false => 0,
    true => ((power_level % 1000) - (power_level % 100)) / 100,
  };

  hundreds_digit - 5
}

// the current solution, with `--release`, it runs in ~2s but in debug it takes a few minutes for
// exercise 2. optimize this function by using: https://en.wikipedia.org/wiki/Summed-area_table
fn calc_top_left_coord_of_max_power_level(
  serial_number: SerialNumber,
  square_size: Option<usize>,
) -> (Coord, usize) {
  let mut power_levels: Vec<Vec<Unit>> = vec![];
  let mut current_coord: Coord = Coord { x: 0, y: 0 };
  let mut current_total_value = 0;
  let mut current_size = 0;

  for y in 0..300 {
    let mut line = vec![];

    for x in 0..300 {
      let mut coord = Coord { x: x, y: y };
      let value = calc_power_level_for_coord(&mut coord, serial_number);

      line.push(value);
    }

    power_levels.push(line);
  }

  let (size_lower_bound, size_upper_bound) = match square_size {
    Some(v) => (v, v + 1),
    None => (0, 300),
  };

  for y in 0..300 {
    for x in 0..300 {
      let upper_bound = min(size_upper_bound, 300 - max(y, x) + 1);
      let mut prev_value: Option<Unit> = None;

      if size_lower_bound > upper_bound {
        break;
      }

      for size in size_lower_bound..upper_bound {
        let mut total_value = 0;

        if prev_value.is_none() {
          for y_2 in 0..size {
            for x_2 in 0..size {
              total_value += power_levels[y + y_2][x + x_2];
            }
          }
        } else {
          total_value = prev_value.unwrap();

          for y_2 in 0..size {
            total_value += power_levels[y + y_2][x + size - 1];
          }

          for x_2 in 0..(size - 1) {
            total_value += power_levels[y + size - 1][x + x_2];
          }
        }

        prev_value = Some(total_value);

        if current_total_value < total_value {
          current_total_value = total_value;
          current_size = size;
          current_coord = Coord {
            x: x as Unit,
            y: y as Unit,
          };
        }
      }
    }
  }

  (current_coord, current_size)
}

fn main() {
  let (coord, _) = calc_top_left_coord_of_max_power_level(GRID_SERIAL_NUMBER, Some(3));
  let (coord2, size2) = calc_top_left_coord_of_max_power_level(GRID_SERIAL_NUMBER, None);

  println!("Results:");
  println!("- (1) result coord: {:?}", coord);
  println!("- (2) result coord: {:?}, size: {}", coord2, size2);
}

#[cfg(test)]
mod tests {
  use super::*;

  type PowerLevel = Unit;

  fn get_examples_data_1() -> Vec<(Coord, SerialNumber, PowerLevel)> {
    vec![
      (Coord { x: 3, y: 5 }, 8, 4),
      (Coord { x: 122, y: 79 }, 57, -5),
      (Coord { x: 101, y: 153 }, 71, 4),
      (Coord { x: 217, y: 196 }, 39, 0),
    ]
  }

  fn get_examples_data_2() -> Vec<(SerialNumber, Coord)> {
    vec![(18, Coord { x: 33, y: 45 }), (42, Coord { x: 21, y: 61 })]
  }

  fn get_examples_data_3() -> Vec<(SerialNumber, Coord, usize)> {
    vec![
      (18, Coord { x: 90, y: 269 }, 16),
      (42, Coord { x: 232, y: 251 }, 12),
    ]
  }

  #[test]
  fn test_calc_power_level_for_coord() {
    let mut examples = get_examples_data_1();

    for mut example in examples {
      let value = calc_power_level_for_coord(&mut example.0, example.1);

      assert_eq!(value, example.2);
    }
  }

  #[test]
  fn test_calc_top_left_coord_of_max_power_level() {
    let mut examples = get_examples_data_2();

    for mut example in examples {
      let (value, _) = calc_top_left_coord_of_max_power_level(example.0, Some(3));

      assert_eq!(value, example.1);
    }
  }

  #[test]
  fn test_calc_top_left_coord_of_max_power_level_2() {
    let mut examples = get_examples_data_3();

    for mut example in examples {
      let (coord, size) = calc_top_left_coord_of_max_power_level(example.0, None);

      assert_eq!(coord, example.1);
      assert_eq!(size, example.2);
    }
  }
}
