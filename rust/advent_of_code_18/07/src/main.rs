/*

--- Day 7: The Sum of Its Parts ---

You find yourself standing on a snow-covered coastline; apparently, you landed a little off course.
The region is too hilly to see the North Pole from here, but you do spot some Elves that seem to be
trying to unpack something that washed ashore. It's quite cold out, so you decide to risk creating
a paradox by asking them for directions.

"Oh, are you the search party?" Somehow, you can understand whatever Elves from the year 1018
speak; you assume it's Ancient Nordic Elvish. Could the device on your wrist also be a translator?
"Those clothes don't look very warm; take this." They hand you a heavy coat.

"We do need to find our way back to the North Pole, but we have higher priorities at the moment.
You see, believe it or not, this box contains something that will solve all of Santa's
transportation problems - at least, that's what it looks like from the pictures in the
instructions." It doesn't seem like they can read whatever language it's in, but you can: "Sleigh
kit. Some assembly required."

"'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at once!" They start
excitedly pulling more parts out of the box.

The instructions specify a series of steps and requirements about which steps must be finished
before others can begin (your puzzle input). Each step is designated by a single letter. For
example, suppose you have the following instructions:

Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.

Visually, these requirements look like this:


  -->A--->B--
 /    \      \
C      -->D----->E
 \           /
  ---->F-----

Your first goal is to determine the order in which the steps should be completed. If more than one
step is ready, choose the step which is first alphabetically. In this example, the steps would be
completed as follows:

Only C is available, and so it is done first.

Next, both A and F are available. A is first alphabetically, so it is done next.

Then, even though F was available earlier, steps B and D are now also available, and B is the first
alphabetically of the three.

After that, only D and F are available. E is not available because only some of its prerequisites
are complete. Therefore, D is completed next.

F is the only choice, so it is done next.

Finally, E is completed.

So, in this example, the correct order is CABDFE.

In what order should the steps in your instructions be completed?

--- Part Two ---

As you're about to begin construction, four of the Elves offer to help. "The sun will set soon;
it'll go faster if we work together." Now, you need to account for multiple people working on steps
simultaneously. If multiple steps are available, workers should still begin them in alphabetical
order.

Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on.
So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds. No time is required between
steps.

To simplify things for the example, however, suppose you only have help from one Elf (a total of
                                                                                      two workers)
and that each step takes 60 fewer seconds (so that step A takes 1 second and step Z takes 26
                                           seconds). Then, using the same instructions as above,
    this is how each second would be spent:

Second   Worker 1   Worker 2   Done
   0        C          .
   1        C          .
   2        C          .
   3        A          F       C
   4        B          F       CA
   5        B          F       CA
   6        D          F       CAB
   7        D          F       CAB
   8        D          F       CAB
   9        D          .       CABF
  10        E          .       CABFD
  11        E          .       CABFD
  12        E          .       CABFD
  13        E          .       CABFD
  14        E          .       CABFD
  15        .          .       CABFDE

Each row represents one second of time. The Second column identifies how many seconds have passed
as of the beginning of that second. Each worker column shows the step that worker is currently
doing (or . if they are idle). The Done column shows completed steps.

Note that the order of the steps has changed; this is because steps now take time to finish and
multiple workers can begin multiple steps simultaneously.

In this example, it would take 15 seconds for two workers to complete these steps.

With 5 workers and the 60+ second step durations described above, how long will it take to complete
all of the steps?

*/

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::iter::FromIterator;

type DepsGraphItemId = char;

#[derive(Debug, PartialEq, Eq, Clone)]
struct DepsGraphProps {
  dependencies: HashSet<DepsGraphItemId>,
  dependants: HashSet<DepsGraphItemId>,
}

type DepsGraph = HashMap<DepsGraphItemId, DepsGraphProps>;

fn create_deps_graph_from_strs(strs: &mut Vec<&str>) -> DepsGraph {
  let mut graph: DepsGraph = HashMap::new();

  let reg = Regex::new(r"^Step (.) must be finished before step (.) can begin.$").unwrap();

  for str_item in strs {
    let caps = reg.captures(str_item).unwrap();
    let item_id = caps.get(1).unwrap().as_str().chars().next().unwrap();
    let dependant = caps.get(2).unwrap().as_str().chars().next().unwrap();

    {
      let item = graph.entry(item_id).or_insert(DepsGraphProps {
        dependencies: HashSet::new(),
        dependants: HashSet::new(),
      });

      item.dependants.insert(dependant);
    }

    {
      let dependant_item = graph.entry(dependant).or_insert(DepsGraphProps {
        dependencies: HashSet::new(),
        dependants: HashSet::new(),
      });

      dependant_item.dependencies.insert(item_id);
    }
  }

  graph
}

fn get_order_of_graph_instructions(graph: &mut DepsGraph) -> Vec<DepsGraphItemId> {
  let mut graph = graph.clone();
  let mut used_chars: HashSet<DepsGraphItemId> = HashSet::new();
  let mut result: Vec<DepsGraphItemId> = vec![];

  loop {
    let mut items_without_dependencies: Vec<DepsGraphItemId> = vec![];

    for graph_item_id in graph.keys() {
      let graph_item = &graph[graph_item_id];

      if graph_item.dependencies.is_empty() && !used_chars.contains(graph_item_id) {
        items_without_dependencies.push(*graph_item_id);
      }
    }

    if items_without_dependencies.is_empty() {
      break;
    }

    items_without_dependencies.sort();

    let chosen_char = items_without_dependencies.first().unwrap();

    used_chars.insert(*chosen_char);
    result.push(*chosen_char);

    let dependants: Vec<DepsGraphItemId>;

    {
      let graph_item = graph.get_mut(chosen_char).unwrap();
      dependants = graph_item.dependants.iter().cloned().collect();
    }

    for item_id in dependants {
      let dependant_graph_item = graph.get_mut(&item_id).unwrap();

      dependant_graph_item.dependencies.remove(&chosen_char);
    }
  }

  result
}

fn get_input_graph() -> DepsGraph {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let mut lines: Vec<&str> = contents.lines().map(|x| x).collect();

  create_deps_graph_from_strs(&mut lines)
}

fn get_seconds_for_char(c: char) -> usize {
  (c.to_digit(36).unwrap() - 9) as usize
}

fn get_seconds_of_execution_with_n_workers(
  graph: &mut DepsGraph,
  workers: usize,
  extra_secs_per_task: usize,
) -> usize {
  let mut graph = graph.clone();
  let total_items = graph.keys().count();
  let mut used_chars: HashSet<DepsGraphItemId> = HashSet::new();
  let mut ellapsed_seconds = 0;
  let mut temporally_without_items = false;

  #[derive(Debug, PartialEq, Eq, Clone)]
  struct Task {
    c: char,
    completed_seconds: usize,
    total_seconds: usize,
  }

  let mut tasks: Vec<Task> = vec![];

  loop {
    let mut tasks_to_complete: Vec<usize> = vec![];

    for (idx, task) in tasks.iter_mut().enumerate() {
      task.completed_seconds += 1;

      if task.completed_seconds == task.total_seconds {
        tasks_to_complete.push(idx);

        let dependants: Vec<DepsGraphItemId>;

        {
          let graph_item = graph.get_mut(&task.c).unwrap();
          dependants = graph_item.dependants.iter().cloned().collect();
        }

        for item_id in dependants {
          let dependant_graph_item = graph.get_mut(&item_id).unwrap();

          dependant_graph_item.dependencies.remove(&task.c);
        }
      }
    }

    tasks_to_complete.reverse();

    for task_idx in tasks_to_complete {
      temporally_without_items = false;
      tasks.remove(task_idx);
    }

    let current_tasks = tasks.len();
    let free_workers = workers - current_tasks;

    if free_workers > 0 && used_chars.len() < total_items && temporally_without_items != true {
      let mut items_without_dependencies: Vec<DepsGraphItemId> = vec![];

      for graph_item_id in graph.keys() {
        let graph_item = &graph[graph_item_id];

        if graph_item.dependencies.is_empty() && !used_chars.contains(graph_item_id) {
          items_without_dependencies.push(*graph_item_id);
        }
      }

      items_without_dependencies.sort();

      for _ in 0..free_workers {
        if items_without_dependencies.is_empty() {
          temporally_without_items = true;
          break;
        }

        let chosen_char = items_without_dependencies.pop().unwrap();

        used_chars.insert(chosen_char);

        tasks.push(Task {
          c: chosen_char,
          completed_seconds: 0,
          total_seconds: get_seconds_for_char(chosen_char) + extra_secs_per_task,
        })
      }
    } else if current_tasks == 0 {
      break;
    }

    ellapsed_seconds += 1;
  }

  ellapsed_seconds
}

fn main() {
  let mut graph = get_input_graph();
  let order = get_order_of_graph_instructions(&mut graph);
  let seconds = get_seconds_of_execution_with_n_workers(&mut graph, 5, 60);

  println!("Result:");
  println!("- (1) order of instructions: {}", String::from_iter(order));
  println!("- (2) seconds: {}", seconds);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn create_example_graph() -> DepsGraph {
    let mut items = vec![
      "Step C must be finished before step A can begin.",
      "Step C must be finished before step F can begin.",
      "Step A must be finished before step B can begin.",
      "Step A must be finished before step D can begin.",
      "Step B must be finished before step E can begin.",
      "Step D must be finished before step E can begin.",
      "Step F must be finished before step E can begin.",
    ];

    create_deps_graph_from_strs(&mut items)
  }

  #[test]
  fn test_get_order_of_graph_instructions() {
    let mut graph = create_example_graph();
    let order = get_order_of_graph_instructions(&mut graph);

    assert_eq!(String::from_iter(order), "CABDFE");
  }

  #[test]
  fn test_get_seconds_of_execution_with_n_workers() {
    let mut graph = create_example_graph();
    let seconds = get_seconds_of_execution_with_n_workers(&mut graph, 2, 0);

    assert_eq!(seconds, 15);
  }
}
