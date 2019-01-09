/*

--- Day 16: Chronal Classification ---

As you see the Elves defend their hot chocolate successfully, you go back to falling through time.
This is going to become a problem.

If you're ever going to return to your own time, you need to understand how this device on your
wrist works. You have a little while before you reach your next destination, and with a bit of
trial and error, you manage to pull up a programming manual on the device's tiny screen.

According to the manual, the device has four registers (numbered 0 through 3) that can be
manipulated by instructions containing one of 16 opcodes. The registers start with the value 0.

Every instruction consists of four values: an opcode, two inputs (named A and B), and an output
(named C), in that order. The opcode specifies the behavior of the instruction and how the inputs
are interpreted. The output, C, is always treated as a register.

In the opcode descriptions below, if something says "value A", it means to take the number given as
A literally. (This is also called an "immediate" value.) If something says "register A", it means
to use the number given as A to read from (or write to) the register with that number. So, if the
opcode addi adds register A and value B, storing the result in register C, and the instruction addi
0 7 3 is encountered, it would add 7 to the value contained by register 0 and store the sum in
register 3, never modifying registers 0, 1, or 2 in the process.

Many opcodes are similar except for how they interpret their arguments. The opcodes fall into seven
general categories:

Addition:

addr (add register) stores into register C the result of adding register A and register B.
addi (add immediate) stores into register C the result of adding register A and value B.
Multiplication:

mulr (multiply register) stores into register C the result of multiplying register A and register
B.

muli (multiply immediate) stores into register C the result of multiplying register A and value B.

Bitwise AND:

banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and
register B.

bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and
value B.

Bitwise OR:

borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and
register B.

bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and
value B.

Assignment:

setr (set register) copies the contents of register A into register C. (Input B is ignored.)
seti (set immediate) stores value A into register C. (Input B is ignored.)
Greater-than testing:

gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B.
Otherwise, register C is set to 0.

gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B.
Otherwise, register C is set to 0.

gtrr (greater-than register/register) sets register C to 1 if register A is greater than register
B. Otherwise, register C is set to 0.

Equality testing:

eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise,
register C is set to 0.

eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise,
register C is set to 0.

eqrr (equal register/register) sets register C to 1 if register A is equal to register B.
Otherwise, register C is set to 0.

Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the
number. However, you can monitor the CPU to see the contents of the registers before and after
instructions are executed to try to work them out. Each opcode has a number from 0 through 15, but
the manual doesn't say which is which. For example, suppose you capture the following sample:

Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

This sample shows the effect of the instruction 9 2 1 2 on the registers. Before the instruction is
executed, register 0 has value 3, register 1 has value 2, and registers 2 and 3 have value 1. After
the instruction is executed, register 2's value becomes 2.

The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2, B=1, and C=2. Opcode 9
could be any of the 16 opcodes listed above, but only three of them behave in a way that would
cause the result shown in the sample:

Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1 (which has a value of
                                                                              2) produces 2, which
matches the value stored in the output register, register 2.

Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1 produces 2, which matches
the value stored in the output register, register 2.

Opcode 9 could be seti: value 2 matches the value stored in the output register, register 2; the
number given for B is irrelevant.

None of the other opcodes produce the result captured in the sample. Because of this, the sample
above behaves like three opcodes.

You collect many of these samples (the first section of your puzzle input). The manual also
includes a small test program (the second section of your puzzle input) - you can ignore it for
now.

Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more opcodes?

--- Part Two ---

Using the samples you collected, work out the number of each opcode and execute the test program
(the second section of your puzzle input).

What value is contained in register 0 after executing the test program?

*/

extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
enum InstructionType {
  Addr,
  Addi,
  Mulr,
  Muli,
  Bani,
  Banr,
  Borr,
  Bori,
  Eqir,
  Eqri,
  Eqrr,
  Gtir,
  Gtri,
  Gtrr,
  Seti,
  Setr,
}

type Register = [usize; 4];
type Instruction = [usize; 4];
type OpcodesMap = HashMap<usize, InstructionType>;

#[derive(Clone, Copy, Debug)]
struct InstructionSet {
  reg_before: Register,
  reg_after: Register,
  instruction: Instruction,
}

impl InstructionSet {
  fn new_default() -> Self {
    InstructionSet {
      reg_before: [0, 0, 0, 0],
      reg_after: [0, 0, 0, 0],
      instruction: [0, 0, 0, 0],
    }
  }
}

const INSTRUCTION_TYPES: [(&str, InstructionType); 16] = [
  ("addr", InstructionType::Addr),
  ("addi", InstructionType::Addi),
  ("mulr", InstructionType::Mulr),
  ("muli", InstructionType::Muli),
  ("bani", InstructionType::Bani),
  ("banr", InstructionType::Banr),
  ("borr", InstructionType::Borr),
  ("bori", InstructionType::Bori),
  ("eqir", InstructionType::Eqir),
  ("eqri", InstructionType::Eqri),
  ("eqrr", InstructionType::Eqrr),
  ("gtir", InstructionType::Gtir),
  ("gtri", InstructionType::Gtri),
  ("gtrr", InstructionType::Gtrr),
  ("seti", InstructionType::Seti),
  ("setr", InstructionType::Setr),
];

fn run_instruction(
  instruction_type: InstructionType,
  instruction: &Instruction,
  register: &Register,
) -> Option<Register> {
  let mut new_reg = register.to_owned();

  let value_a = instruction[1];
  let value_b = instruction[2];
  let value_c = instruction[3];

  if value_c > 3 {
    return None;
  }

  let register_a = if value_a > 3 {
    None
  } else {
    Some(new_reg[value_a])
  };
  let register_b = if value_b > 3 {
    None
  } else {
    Some(new_reg[value_b])
  };

  let val = match instruction_type {
    // addr (add register) stores into register C the result of adding register A and register B.
    InstructionType::Addr => {
      if register_a.is_some() && register_b.is_some() {
        Some(register_a.unwrap() + register_b.unwrap())
      } else {
        None
      }
    }
    // addi (add immediate) stores into register C the result of adding register A and value B.
    InstructionType::Addi => register_a.and_then(|reg_a| Some(reg_a + value_b)),
    // mulr (multiply register) stores into register C the result of multiplying register A and register B.
    InstructionType::Mulr => register_a
      .and(register_b)
      .and_then(|reg_b| Some(register_a.unwrap() * reg_b)),
    // muli (multiply immediate) stores into register C the result of multiplying register A and value B.
    InstructionType::Muli => register_a.and_then(|reg_a| Some(reg_a * value_b)),
    // banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
    InstructionType::Banr => register_a
      .and(register_b)
      .and_then(|reg_b| Some(register_a.unwrap() & reg_b)),
    // bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
    InstructionType::Bani => register_a.and_then(|reg_a| Some(reg_a & value_b)),
    // eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
    InstructionType::Eqir => {
      register_b.and_then(|reg_b| if value_a == reg_b { Some(1) } else { Some(0) })
    }
    // eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
    InstructionType::Eqri => {
      register_a.and_then(|reg_a| if reg_a == value_b { Some(1) } else { Some(0) })
    }
    // eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
    InstructionType::Eqrr => register_a.and(register_b).and_then(|reg_b| {
      if register_a.unwrap() == reg_b {
        Some(1)
      } else {
        Some(0)
      }
    }),
    // gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
    InstructionType::Gtir => {
      register_b.and_then(|reg_b| if value_a > reg_b { Some(1) } else { Some(0) })
    }
    // gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
    InstructionType::Gtri => {
      register_a.and_then(|reg_a| if reg_a > value_b { Some(1) } else { Some(0) })
    }
    // gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
    InstructionType::Gtrr => register_a.and(register_b).and_then(|reg_b| {
      if register_a.unwrap() > reg_b {
        Some(1)
      } else {
        Some(0)
      }
    }),
    // borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
    InstructionType::Borr => register_a
      .and(register_b)
      .and_then(|reg_b| Some(register_a.unwrap() | reg_b)),
    // bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
    InstructionType::Bori => register_a.and_then(|reg_a| Some(reg_a | value_b)),
    // seti (set immediate) stores value A into register C. (Input B is ignored.)
    InstructionType::Seti => Some(value_a),
    // setr (set register) copies the contents of register A into register C. (Input B is ignored.)
    InstructionType::Setr => register_a,
  };

  val?;

  new_reg[value_c] = val.unwrap();

  Some(new_reg)
}

fn get_are_registers_equal(reg_1: &Register, reg_2: &Register) -> bool {
  reg_1[0] == reg_2[0] && reg_1[1] == reg_2[1] && reg_1[2] == reg_2[2] && reg_1[3] == reg_2[3]
}

fn loop_instructions_and_get_passing(
  instruction: &Instruction,
  register_in: &Register,
  register_out: &Register,
) -> HashSet<InstructionType> {
  let mut passing_instructions: HashSet<InstructionType> = HashSet::new();

  for instruction_tuple in &INSTRUCTION_TYPES {
    let (_, instruction_type) = instruction_tuple;
    let result = run_instruction(*instruction_type, instruction, register_in);

    if result.is_some() && get_are_registers_equal(&result.unwrap(), register_out) {
      passing_instructions.insert(*instruction_type);
    }
  }

  passing_instructions
}

fn get_instruction_sets_passing_three_or_more(instruction_sets: &[InstructionSet]) -> usize {
  let mut total = 0;

  for instruction_set in instruction_sets {
    let result = loop_instructions_and_get_passing(
      &instruction_set.instruction,
      &instruction_set.reg_before,
      &instruction_set.reg_after,
    );

    if result.len() >= 3 {
      total += 1;
    }
  }

  total
}

fn get_instruction_regex() -> Regex {
  Regex::new(r"^(\d+) (\d+) (\d+) (\d+)$").unwrap()
}

fn get_input_instruction_sets() -> Vec<InstructionSet> {
  let mut file = File::open("src/input_1.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let before_regex = Regex::new(r"^Before: \[(.*), (.*), (.*), (.*)\]$").unwrap();
  let after_regex = Regex::new(r"^After:  \[(.*), (.*), (.*), (.*)\]$").unwrap();
  let instruction_regex = get_instruction_regex();

  let mut instruction_sets: Vec<InstructionSet> = vec![];
  let mut instruction_set = InstructionSet::new_default();

  for (idx, line) in contents.lines().enumerate() {
    let remain = idx % 4;
    if remain == 0 {
      let caps = before_regex.captures(line).unwrap();

      instruction_set = InstructionSet::new_default();
      instruction_set.reg_before = [
        caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      ];
    } else if remain == 1 {
      let caps = instruction_regex.captures(line).unwrap();

      instruction_set.instruction = [
        caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      ];
    } else if remain == 2 {
      let caps = after_regex.captures(line).unwrap();

      instruction_set.reg_after = [
        caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      ];
    } else if remain == 3 {
      instruction_sets.push(instruction_set);
    }
  }

  instruction_sets
}

fn get_test_program_lines() -> Vec<Instruction> {
  let mut file = File::open("src/input_2.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  let instruction_regex = get_instruction_regex();

  let mut instructions: Vec<Instruction> = vec![];

  for line in contents.lines() {
    let caps = instruction_regex.captures(line).unwrap();

    let instruction: Instruction = [
      caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
      caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
      caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
      caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
    ];

    instructions.push(instruction);
  }

  instructions
}

fn find_op_codes(instruction_sets: &[InstructionSet]) -> OpcodesMap {
  let mut opcodes: OpcodesMap = HashMap::new();
  let mut found: HashSet<InstructionType> = HashSet::new();
  let total = &INSTRUCTION_TYPES.len();

  while found.len() < *total {
    for instruction_set in instruction_sets {
      let result = loop_instructions_and_get_passing(
        &instruction_set.instruction,
        &instruction_set.reg_before,
        &instruction_set.reg_after,
      );

      let found_cloned = found.clone();
      let intersection: HashSet<&InstructionType> = result.difference(&found_cloned).collect();

      let instruction_num = instruction_set.instruction[0];

      if intersection.len() == 1 && opcodes.get(&instruction_num).is_none() {
        let instruction_type = **intersection.iter().nth(0).unwrap();

        opcodes.insert(instruction_num, instruction_type);
        found.insert(instruction_type);
      }
    }
  }

  opcodes
}

fn run_instructions(
  register: &Register,
  instructions: &[Instruction],
  opcodes: &OpcodesMap,
) -> Register {
  let mut result_register = *register;

  for instruction in instructions {
    let instruction_type = opcodes.get(&instruction[0]).unwrap();
    let new_register = run_instruction(*instruction_type, &instruction, &result_register);

    result_register = new_register.unwrap();
  }

  result_register
}

fn main() {
  let instruction_sets = get_input_instruction_sets();
  let result_first_exercise = get_instruction_sets_passing_three_or_more(&instruction_sets);
  let opcodes = find_op_codes(&instruction_sets);
  let test_program_lines = get_test_program_lines();
  let initial_register = [0, 0, 0, 0];
  let result_register = run_instructions(&initial_register, &test_program_lines, &opcodes);

  println!("Results");
  println!("- (1) first exercise: {:?}", result_first_exercise);
  println!("- (2) second exercise: {:?}", result_register[0]);
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_run_instruction() {
    assert_eq!(
      run_instruction(InstructionType::Addr, &[0, 1, 3, 2], &[0, 1, 0, 3]),
      Some([0, 1, 4, 3])
    );
    assert_eq!(
      run_instruction(InstructionType::Addi, &[0, 1, 10, 2], &[0, 1, 0, 3]),
      Some([0, 1, 11, 3])
    );
    assert_eq!(
      run_instruction(InstructionType::Mulr, &[0, 1, 3, 2], &[0, 1, 0, 3]),
      Some([0, 1, 3, 3])
    );
    assert_eq!(
      run_instruction(InstructionType::Muli, &[0, 1, 10, 2], &[0, 1, 0, 3]),
      Some([0, 1, 10, 3])
    );
    assert_eq!(
      run_instruction(InstructionType::Banr, &[0, 1, 2, 3], &[0, 3, 3, 0]),
      Some([0, 3, 3, 3])
    );
  }
}
