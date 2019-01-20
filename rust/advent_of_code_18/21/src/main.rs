/*

--- Day 21: Chronal Conversion ---

You should have been watching where you were going, because as you wander the new North Pole base,
you trip and fall into a very deep hole!

Just kidding. You're falling through time again.

If you keep up your current pace, you should have resolved all of the temporal anomalies by the
next time the device activates. Since you have very little interest in browsing history in 500-year
increments for the rest of your life, you need to find a way to get back to your present time.

After a little research, you discover two important facts about the behavior of the device:

First, you discover that the device is hard-wired to always send you back in time in 500-year
increments. Changing this is probably not feasible.

Second, you discover the activation system (your puzzle input) for the time travel module.
Currently, it appears to run forever without halting.

If you can cause the activation system to halt at a specific moment, maybe you can make the device
send you so far back in time that you cause an integer underflow in time itself and wrap around
back to your current time!

The device executes the program as specified in manual section one and manual section two.

Your goal is to figure out how the program works and cause it to halt. You can only control
register 0; every other register begins at 0 as usual.

Because time travel is a dangerous activity, the activation system begins with a few instructions
which verify that bitwise AND (via bani) does a numeric operation and not an operation as if the
inputs were interpreted as strings. If the test fails, it enters an infinite loop re-running the
test instead of allowing the program to execute normally. If the test passes, the program
continues, and assumes that all other bitwise operations (banr, bori, and borr) also interpret
their inputs as numbers. (Clearly, the Elves who wrote this system were worried that someone might
introduce a bug while trying to emulate this system with a scripting language.)

What is the lowest non-negative integer value for register 0 that causes the program to halt after
executing the fewest instructions? (Executing the same instruction multiple times counts as
multiple instructions executed.)

--- Part Two ---

In order to determine the timing window for your underflow exploit, you also need an upper bound:

What is the lowest non-negative integer value for register 0 that causes the program to halt after
executing the most instructions? (The program must actually halt; running forever does not count as
halting.)

*/

extern crate regex;

use regex::Regex;
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

type Register = [usize; 6];
type Instruction = (InstructionType, usize, usize, usize);

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

fn get_instruction_type_from_str(text: &str) -> Option<InstructionType> {
  for set in INSTRUCTION_TYPES.iter() {
    if text == set.0 {
      return Some(set.1);
    }
  }

  None
}

fn run_instruction(instruction: &Instruction, register: &Register) -> Option<Register> {
  let mut new_reg = register.to_owned();

  let value_a = instruction.1;
  let value_b = instruction.2;
  let value_c = instruction.3;

  if value_c > 5 {
    return None;
  }

  let register_a = if value_a > 5 {
    None
  } else {
    Some(new_reg[value_a])
  };
  let register_b = if value_b > 5 {
    None
  } else {
    Some(new_reg[value_b])
  };

  let instruction_type = instruction.0;

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

struct Program {
  instruction_pointer: usize,
  instructions: Vec<Instruction>,
}

impl Program {
  fn new_from_text(text: &str) -> Self {
    let ip_reg = Regex::new(r"^#ip (.+)$").unwrap();
    let instruction_reg = Regex::new(r"^(.+) (.+) (.+) (.+)$").unwrap();
    let lines: Vec<&str> = text.lines().collect();
    let mut instructions: Vec<Instruction> = vec![];

    let ip = ip_reg
      .captures(lines[0])
      .unwrap()
      .get(1)
      .unwrap()
      .as_str()
      .parse::<usize>()
      .unwrap();

    for line in lines.iter().skip(1) {
      let caps = instruction_reg.captures(line).unwrap();
      let instruction_name = caps.get(1).unwrap().as_str();
      let instruction_type = get_instruction_type_from_str(&instruction_name).unwrap();
      let instruction = (
        instruction_type,
        caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(3).unwrap().as_str().parse::<usize>().unwrap(),
        caps.get(4).unwrap().as_str().parse::<usize>().unwrap(),
      );
      instructions.push(instruction);
    }

    Program {
      instruction_pointer: ip,
      instructions,
    }
  }

  fn run(&self, should_stop_on_first: bool) -> usize {
    let mut curr_reg = [0, 0, 0, 0, 0, 0];
    let instructions_len = self.instructions.len();
    let mut reg_values: Vec<usize> = vec![];

    loop {
      let pointer_value = curr_reg[self.instruction_pointer];

      if pointer_value >= instructions_len {
        break;
      }

      // hardcoded due to instructions
      if pointer_value == 28 {
        if should_stop_on_first {
          return curr_reg[3];
        } else if reg_values.contains(&curr_reg[3]) {
          return reg_values.pop().unwrap();
        } else {
          reg_values.push(curr_reg[3]);
        }
      }

      let instruction = self.instructions[pointer_value];
      let next_reg = run_instruction(&instruction, &curr_reg).unwrap();

      curr_reg = next_reg;
      curr_reg[self.instruction_pointer] += 1;
    }

    0
  }

  fn get_instruction_text(&self, instruction: &Instruction, instruction_idx: usize) -> String {
    let mut instruction_text = String::new();
    let reg_letters = vec!['A', 'B', 'C', 'D', 'E', 'F'];

    instruction_text.push_str(&format!("{:02} -> ", instruction_idx));

    if instruction.3 == self.instruction_pointer {
      instruction_text.push_str("GOTO ");
    } else {
      instruction_text.push_str(&format!("{} = ", reg_letters[instruction.3]));
    }

    match instruction.0 {
      InstructionType::Addi => {
        if instruction.1 == self.instruction_pointer {
          instruction_text.push_str(&format!("{}", instruction_idx + instruction.2));
        } else {
          instruction_text.push_str(&format!(
            "{} + {}",
            reg_letters[instruction.1], instruction.2
          ));
        }
      }
      InstructionType::Muli => {
        if instruction.1 == self.instruction_pointer {
          instruction_text.push_str(&format!("{}", instruction_idx * instruction.2));
        } else {
          instruction_text.push_str(&format!(
            "{} * {}",
            reg_letters[instruction.1], instruction.2
          ));
        }
      }
      InstructionType::Addr => {
        if instruction.1 == self.instruction_pointer {
          instruction_text.push_str(&format!("+{}", reg_letters[instruction.2]));
        } else if instruction.2 == self.instruction_pointer {
          instruction_text.push_str(&format!("+{}", reg_letters[instruction.1]));
        } else {
          instruction_text.push_str(&format!(
            "{} + {}",
            reg_letters[instruction.1], reg_letters[instruction.2]
          ));
        }
      }
      InstructionType::Seti => {
        instruction_text.push_str(&format!("{}", instruction.1));
      }
      InstructionType::Setr => {
        instruction_text.push_str(&format!("{}", reg_letters[instruction.1]));
      }
      InstructionType::Eqri => {
        instruction_text.push_str(&format!(
          "{} == {} ? 1 : 0",
          reg_letters[instruction.1], instruction.2
        ));
      }
      InstructionType::Eqrr => {
        instruction_text.push_str(&format!(
          "{} == {} ? 1 : 0",
          reg_letters[instruction.1], reg_letters[instruction.2]
        ));
      }
      InstructionType::Bani => {
        instruction_text.push_str(&format!(
          "{} & {}",
          reg_letters[instruction.1], instruction.2
        ));
      }
      InstructionType::Bori => {
        instruction_text.push_str(&format!(
          "{} | {}",
          reg_letters[instruction.1], instruction.2
        ));
      }
      InstructionType::Gtir => {
        instruction_text.push_str(&format!(
          "{} > {} ? 1 : 0",
          instruction.1, reg_letters[instruction.2]
        ));
      }
      InstructionType::Gtrr => {
        instruction_text.push_str(&format!(
          "{} > {} ? 1 : 0",
          reg_letters[instruction.1], reg_letters[instruction.2]
        ));
      }
      _ => {
        instruction_text.push_str(&format!("{:?}", instruction));
      }
    }

    instruction_text
  }

  #[allow(dead_code)]
  fn print_program(&self) {
    println!("----");
    println!("A:0, B:1, C:2, D:3, E:4, F:5");
    for (idx, instruction) in self.instructions.clone().iter().enumerate() {
      let instruction_text = self.get_instruction_text(&instruction, idx);
      println!("{}", instruction_text);
    }
    println!("----");
  }
}

fn get_program() -> Program {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  Program::new_from_text(&contents)
}

fn main() {
  let program = get_program();

  let part_1_result = program.run(true);
  let part_2_result = program.run(false); // this takes around one minute

  println!("Results:");
  println!("- (1) min reg 0: {}", part_1_result);
  println!("- (2) min reg 0: {}", part_2_result);
}
