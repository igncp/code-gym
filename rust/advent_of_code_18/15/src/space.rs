use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
pub struct Coord {
  pub x: usize,
  pub y: usize,
}

impl Coord {
  pub fn new(x: usize, y: usize) -> Self {
    Coord { x, y }
  }

  pub fn get_is_next_to_coord(&self, other: &Coord) -> bool {
    if self.x == other.x && (self.y as i32 - other.y as i32).abs() == 1 {
      return true;
    }

    if self.y == other.y && (self.x as i32 - other.x as i32).abs() == 1 {
      return true;
    }

    false
  }
}

impl Ord for Coord {
  fn cmp(&self, other: &Coord) -> Ordering {
    match self.y.cmp(&other.y) {
      Ordering::Equal => self.x.cmp(&other.x),
      v => v,
    }
  }
}

impl PartialOrd for Coord {
  fn partial_cmp(&self, other: &Coord) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TerrainType {
  Empty,
  OtherObstacle,
  Wall,
}

#[derive(Debug)]
pub struct Dimensions {
  pub width: usize,
  pub height: usize,
}

pub type MapTopology = HashMap<Coord, TerrainType>;

#[derive(Debug)]
pub struct Map {
  pub topology: MapTopology,
  pub dimensions: Dimensions,
}

impl Map {
  pub fn new(chars_vecs: &[Vec<char>]) -> Self {
    let mut topology: MapTopology = HashMap::new();
    let dimensions = Dimensions {
      height: chars_vecs.iter().count(),
      width: chars_vecs[0].iter().count(),
    };

    for (y, line) in chars_vecs.iter().enumerate() {
      for (x, ch) in line.iter().enumerate() {
        let coord = Coord::new(x, y);
        let terrain = if ch == &'#' {
          TerrainType::Wall
        } else {
          TerrainType::Empty
        };

        topology.insert(coord, terrain);
      }
    }

    Self {
      topology,
      dimensions,
    }
  }

  pub fn set_all_obstacles(&mut self, obstacles_coords: &[Coord]) {
    let topology_clone = self.topology.clone();
    let coords = topology_clone.keys();

    for topology_coord in coords {
      let topology_val = &topology_clone[topology_coord];

      if *topology_val == TerrainType::OtherObstacle {
        self.topology.insert(*topology_coord, TerrainType::Empty);
      }
    }

    for obstacle_coord in obstacles_coords {
      self
        .topology
        .insert(*obstacle_coord, TerrainType::OtherObstacle);
    }
  }

  pub fn get_all_ranges(&self, coords: &[Coord]) -> HashSet<Coord> {
    let mut ranges: HashSet<Coord> = HashSet::new();

    fn insert_coord_if_possible(map: &Map, ranges: &mut HashSet<Coord>, x: usize, y: usize) {
      let coord = Coord { x, y };

      let range = if map.topology[&coord] == TerrainType::Empty {
        Some(coord)
      } else {
        None
      };

      if range.is_some() {
        ranges.insert(range.unwrap());
      }
    }

    for coord in coords {
      if coord.x != 0 {
        insert_coord_if_possible(&self, &mut ranges, coord.x - 1, coord.y);
      };
      if coord.y != 0 {
        insert_coord_if_possible(&self, &mut ranges, coord.x, coord.y - 1);
      };
      if coord.x < &self.dimensions.width - 1 {
        insert_coord_if_possible(&self, &mut ranges, coord.x + 1, coord.y);
      };
      if coord.y < &self.dimensions.height - 1 {
        insert_coord_if_possible(&self, &mut ranges, coord.x, coord.y + 1);
      };
    }

    ranges
  }
}

#[cfg(test)]
mod tests {}
