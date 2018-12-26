pub fn add_two(a: i32, b: u32) -> i32 {
  a + b as i32
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn sums() {
    let result = add_two(2, 1);

    assert_eq!(result, 3);
  }
}
