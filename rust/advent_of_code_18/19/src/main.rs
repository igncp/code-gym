/*

--- Day 19: Go With The Flow ---

With the Elves well on their way constructing the North Pole base, you turn your attention back to
understanding the inner workings of programming the device.

You can't help but notice that the device's opcodes don't contain any flow control like jump
instructions. The device's manual goes on to explain:

"In programs where flow control is required, the instruction pointer can be bound to a register so
that it can be manipulated directly. This way, setr/seti can function as absolute jumps, addr/addi
can function as relative jumps, and other opcodes can cause truly fascinating effects."

This mechanism is achieved through a declaration like #ip 1, which would modify register 1 so that
accesses to it let the program indirectly access the instruction pointer itself. To compensate for
this kind of binding, there are now six registers (numbered 0 through 5); the five not bound to the
instruction pointer behave as normal. Otherwise, the same rules apply as the last time you worked
with this device.

When the instruction pointer is bound to a register, its value is written to that register just
before each instruction is executed, and the value of that register is written back to the
instruction pointer immediately after each instruction finishes execution. Afterward, move to the
next instruction by adding one to the instruction pointer, even if the value in the instruction
pointer was just updated by an instruction. (Because of this, instructions must effectively set the
instruction pointer to the instruction before the one they want executed next.)

The instruction pointer is 0 during the first instruction, 1 during the second, and so on. If the
instruction pointer ever causes the device to attempt to load an instruction outside the
instructions defined in the program, the program instead immediately halts. The instruction pointer
starts at 0.

It turns out that this new information is already proving useful: the CPU in the device is not very
powerful, and a background process is occupying most of its time. You dump the background process'
declarations and instructions to a file (your puzzle input), making sure to use the names of the
opcodes rather than the numbers.

For example, suppose you have the following program:

#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5

When executed, the following instructions are executed. Each line contains the value of the
instruction pointer at the time the instruction started, the values of the six registers before
executing the instructions (in square brackets), the instruction itself, and the values of the six
registers after executing the instruction (also in square brackets).

ip=0 [0, 0, 0, 0, 0, 0] seti 5 0 1 [0, 5, 0, 0, 0, 0]
ip=1 [1, 5, 0, 0, 0, 0] seti 6 0 2 [1, 5, 6, 0, 0, 0]
ip=2 [2, 5, 6, 0, 0, 0] addi 0 1 0 [3, 5, 6, 0, 0, 0]
ip=4 [4, 5, 6, 0, 0, 0] setr 1 0 0 [5, 5, 6, 0, 0, 0]
ip=6 [6, 5, 6, 0, 0, 0] seti 9 0 5 [6, 5, 6, 0, 0, 9]

In detail, when running this program, the following events occur:

The first line (#ip 0) indicates that the instruction pointer should be bound to register 0 in this
program. This is not an instruction, and so the value of the instruction pointer does not change
during the processing of this line.

The instruction pointer contains 0, and so the first instruction is executed (seti 5 0 1). It
updates register 0 to the current instruction pointer value (0), sets register 1 to 5, sets the
instruction pointer to the value of register 0 (which has no effect, as the instruction did not
modify register 0), and then adds one to the instruction pointer.

The instruction pointer contains 1, and so the second instruction, seti 6 0 2, is executed. This is
very similar to the instruction before it: 6 is stored in register 2, and the instruction pointer
is left with the value 2.

The instruction pointer is 2, which points at the instruction addi 0 1 0. This is like a relative
jump: the value of the instruction pointer, 2, is loaded into register 0. Then, addi finds the
result of adding the value in register 0 and the value 1, storing the result, 3, back in register
0. Register 0 is then copied back to the instruction pointer, which will cause it to end up 1
larger than it would have otherwise and skip the next instruction (addr 1 2 3) entirely. Finally, 1
is added to the instruction pointer.

The instruction pointer is 4, so the instruction setr 1 0 0 is run. This is like an absolute jump:
it copies the value contained in register 1, 5, into register 0, which causes it to end up in the
instruction pointer. The instruction pointer is then incremented, leaving it at 6.

The instruction pointer is 6, so the instruction seti 9 0 5 stores 9 into register 5. The
instruction pointer is incremented, causing it to point outside the program, and so the program
ends.

What value is left in register 0 when the background process halts?

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

  return None;
}

#[derive(Debug)]
struct HistoryItem {
  instruction: Instruction,
  instruction_pointer: usize,
  reg_after: Register,
  reg_before: Register,
}

type History = Vec<HistoryItem>;

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

  fn run(&mut self) -> History {
    let mut history: History = vec![];
    let mut curr_reg = [0, 0, 0, 0, 0, 0];
    let instructions_len = self.instructions.len();

    loop {
      let pointer_value = curr_reg[self.instruction_pointer];

      if pointer_value >= instructions_len {
        break;
      }

      let instruction = self.instructions[pointer_value];
      let next_reg = run_instruction(&instruction, &curr_reg).unwrap();

      let history_item = HistoryItem {
        instruction: instruction,
        instruction_pointer: self.instruction_pointer,
        reg_before: curr_reg.clone(),
        reg_after: next_reg.clone(),
      };

      history.push(history_item);

      curr_reg = next_reg.clone();
      curr_reg[self.instruction_pointer] = curr_reg[self.instruction_pointer] + 1;
    }

    history
  }

  fn run_optimized(&mut self) -> History {
    let mut history: History = vec![];
    let mut curr_reg = [1, 0, 0, 0, 0, 0];
    let instructions_len = self.instructions.len();

    let first_val = 10551267;

    loop {
      let pointer_value = curr_reg[self.instruction_pointer];

      if pointer_value >= instructions_len {
        break;
      }

      let instruction = self.instructions[pointer_value];

      let next_reg = run_instruction(&instruction, &curr_reg).unwrap();

      println!("next_reg {:?}", next_reg);
      println!("instruction {:?}", instruction);
      println!("");

      let history_item = HistoryItem {
        instruction: instruction,
        instruction_pointer: self.instruction_pointer,
        reg_before: curr_reg.clone(),
        reg_after: next_reg.clone(),
      };

      history.push(history_item);

      curr_reg = next_reg.clone();
      curr_reg[self.instruction_pointer] = curr_reg[self.instruction_pointer] + 1;
    }

    history
  }
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

fn get_program() -> Program {
  let mut file = File::open("src/input.txt").expect("Unable to open the file");
  let mut contents = String::new();
  file
    .read_to_string(&mut contents)
    .expect("Unable to read the file");

  Program::new_from_text(&contents)
}

fn main() {
  let mut program = get_program();
  let history = program.run();
  let last_item = history.last().unwrap().reg_after[0];

  // For part 2 it needs to understand what the program is doing

  /*

  Repeated instructions:
    (Seti, 2, 2, 5)
    (Mulr, 3, 2, 4)
    (Eqrr, 4, 1, 4)
    (Addr, 4, 5, 5)
    (Addi, 5, 1, 5)
    (Addi, 2, 1, 2)
    (Gtrr, 2, 1, 4)
    (Addr, 5, 4, 5)

  Register names: A,B,C,D,E,ip

  Flow:
    if B > C:
      ip = 2;
      E = D * C;
      D = D == B;
      ip = ip + E + 1;
      C =  C + 1;
    else:
      ip = ip + 1;
     */

  println!("Results");
  println!("- (1) value on register 0: {}", last_item);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn get_example_data() -> Program {
    let text = "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5"
      .to_string();

    Program::new_from_text(&text)
  }

  #[test]
  fn test_program_new_from_text() {
    let program = get_example_data();

    assert_eq!(program.instruction_pointer, 0);
    assert_eq!(program.instructions[0], (InstructionType::Seti, 5, 0, 1));
    assert_eq!(program.instructions.len(), 7);
  }

  #[test]
  fn test_program_run() {
    let mut program = get_example_data();
    let history = program.run();

    assert_eq!(history.len(), 5);
    assert_eq!(history.last().unwrap().reg_after[0], 6)
  }
}
