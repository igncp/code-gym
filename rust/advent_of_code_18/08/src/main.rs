/*

--- Day 8: Memory Maneuver ---

The sleigh is much easier to pull than you'd expect for something its weight. Unfortunately,
neither you nor the Elves know which way the North Pole is from here.

You check your wrist device for anything that might help. It seems to have some kind of navigation
system! Activating the navigation system produces more bad news: "Failed to start navigation
system. Could not read software license file."

The navigation system's license file consists of a list of numbers (your puzzle input). The numbers
define a data structure which, when processed, produces some kind of tree that can be used to
calculate the license number.

The tree is made up of nodes; a single, outermost node forms the tree's root, and it contains all
other nodes in the tree (or contains nodes that contain nodes, and so on).

Specifically, a node consists of:

A header, which is always exactly two numbers:

The quantity of child nodes.
The quantity of metadata entries.
Zero or more child nodes (as specified in the header).
One or more metadata entries (as specified in the header).

Each child node is itself a node that has its own header, child nodes, and metadata. For example:

2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----

In this example, each node of the tree is also marked with an underline starting with a letter for
easier identification. In it, there are four nodes:

A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
C, which has 1 child node (D) and 1 metadata entry (2).
D, which has 0 child nodes and 1 metadata entry (99).

The first check done on the license file is to simply add up all of the metadata entries. In this
example, that sum is 1+1+2+10+11+12+2+99=138.

What is the sum of all metadata entries?

--- Part Two ---

The second check is slightly more complicated: you need to find the value of the root node (A in
the example above).

The value of a node depends on whether it has child nodes.

If a node has no child nodes, its value is the sum of its metadata entries. So, the value of node B
is 10+11+12=33, and the value of node D is 99.

However, if a node does have child nodes, the metadata entries become indexes which refer to those
child nodes. A metadata entry of 1 refers to the first child node, 2 to the second, 3 to the third,
and so on. The value of this node is the sum of the values of the child nodes referenced by the
metadata entries. If a referenced child node does not exist, that reference is skipped. A child
node can be referenced multiple time and counts each time it is referenced. A metadata entry of 0
does not refer to any child node.

For example, again using the above nodes:

Node C has one metadata entry, 2. Because node C has only one child node, 2 references a child node
which does not exist, and so the value of node C is 0.

Node A has three metadata entries: 1, 1, and 2. The 1 references node A's first child node, B, and
the 2 references node A's second child node, C. Because node B has a value of 33 and node C has a
value of 0, the value of node A is 33+33+0=66.

So, in this example, the value of the root node is 66.

What is the value of the root node?

*/

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

fn get_numbers() -> Vec<usize> {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let reg = Regex::new(r"(\d+)").unwrap();
  let mut nums = vec![];

  for cap in reg.captures_iter(contents.as_str()) {
    let num = &cap[1].parse::<usize>().unwrap();

    nums.push(*num);
  }

  nums
}

type TreeNodeId = usize;

#[derive(Debug)]
struct TreeNode {
  id: TreeNodeId,
  metadata_nums_len: usize,
  metadata_nums_sum: usize,
  node_value: usize,
  childs_num: usize,
  childs_ids: Vec<TreeNodeId>,
  is_root: bool,
}

type Tree = HashMap<TreeNodeId, TreeNode>;

fn build_tree(nums: &mut Vec<usize>) -> Tree {
  let mut nums = nums;
  let mut tree: Tree = HashMap::new();

  build_tree_rec(&mut nums, &mut tree, 0, true);

  tree
}

fn build_tree_rec(
  nums: &mut Vec<usize>,
  tree: &mut Tree,
  id_count: usize,
  is_root: bool,
) -> (TreeNodeId, usize) {
  let mut current_id = id_count;
  let mut tree = tree;
  let nums_len = nums.len();
  let nums_used;

  let mut current_tree_node = TreeNode {
    id: 0,
    metadata_nums_sum: 0,
    metadata_nums_len: nums[1],
    node_value: 0,
    childs_num: nums[0],
    childs_ids: vec![],
    is_root: is_root,
  };

  if current_tree_node.childs_num == 0 {
    nums_used = 2 + current_tree_node.metadata_nums_len;

    for idx in 2..nums_used {
      current_tree_node.metadata_nums_sum += nums[idx];
    }

    current_tree_node.node_value = current_tree_node.metadata_nums_sum;
  } else {
    let mut current_idx = 2;
    for _ in 0..current_tree_node.childs_num {
      let mut nums_sub_set = nums.get(current_idx..nums_len).unwrap().to_vec();

      let (child_id, child_used_nums) =
        build_tree_rec(&mut nums_sub_set, &mut tree, current_id, false);

      current_tree_node.childs_ids.push(child_id);

      current_id = child_id;
      current_idx += child_used_nums;
    }

    nums_used = current_idx + current_tree_node.metadata_nums_len;

    for metadata_num_idx in current_idx..nums_used {
      let metadata_val = nums[metadata_num_idx];
      current_tree_node.metadata_nums_sum += metadata_val;

      if metadata_val <= current_tree_node.childs_num {
        let child_id = current_tree_node.childs_ids.get(metadata_val - 1).unwrap();
        let child = tree.get(child_id).unwrap();

        current_tree_node.node_value += child.node_value;
      }
    }
  }

  current_id += 1;
  current_tree_node.id = current_id;

  tree.insert(current_id, current_tree_node);

  (current_id, nums_used)
}

fn get_tree_first_check(tree: &mut Tree) -> usize {
  let mut sum = 0;

  for node in tree.values() {
    sum += node.metadata_nums_sum;
  }

  sum
}

fn get_tree_second_check(tree: &mut Tree) -> usize {
  for node in tree.values() {
    if node.is_root {
      return node.node_value;
    }
  }

  0
}

fn main() {
  let mut numbers = get_numbers();
  let mut tree = build_tree(&mut numbers);

  let first_check = get_tree_first_check(&mut tree);
  let second_check = get_tree_second_check(&mut tree);

  println!("Result:");
  println!("- (1) first total sum: {}", first_check);
  println!("- (2) second total sum: {}", second_check);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn create_example_nums() -> Vec<usize> {
    vec![2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
  }

  #[test]
  fn test_get_tree_first_check() {
    let mut nums = create_example_nums();
    let mut tree = build_tree(&mut nums);
    let first_check = get_tree_first_check(&mut tree);

    assert_eq!(first_check, 138);
  }

  #[test]
  fn test_get_tree_first_check_2() {
    let mut nums = vec![2, 3, 1, 3, 0, 1, 1, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2];
    let mut tree = build_tree(&mut nums);
    let first_check = get_tree_first_check(&mut tree);

    assert_eq!(first_check, 139);
  }

  #[test]
  fn test_get_tree_second_check() {
    let mut nums = create_example_nums();
    let mut tree = build_tree(&mut nums);
    let second = get_tree_second_check(&mut tree);

    assert_eq!(second, 66);
  }
}
