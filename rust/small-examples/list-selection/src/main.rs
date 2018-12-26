extern crate termion;

use std::io::{stdin, stdout, Write};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

fn clear_console() {
  let mut stdout = stdout().into_raw_mode().unwrap();

  writeln!(
    stdout,
    "{}{}{}",
    termion::clear::All,
    termion::cursor::Goto(1, 1),
    termion::cursor::Hide,
  )
  .unwrap();
}

fn write_list(opts: &Vec<&str>, sel: usize) {
  clear_console();

  let mut stdout = stdout();
  let opts_with_sel: Vec<String> = opts
    .iter()
    .enumerate()
    .map(|(i, opt)| match i == sel {
      true => "-> ".to_string() + opt,
      false => "   ".to_string() + opt,
    })
    .collect();
  let full_stdout_str = opts_with_sel.join("\n\r");

  write!(stdout, "{}", full_stdout_str + "\n\r").unwrap();
}

enum Action {
  Add,
  Substract,
}

fn get_next_selected(current: usize, list: &Vec<&str>, action: Action) -> usize {
  let mut next: usize = current;

  match action {
    Action::Add => {
      if next < list.len() - 1 {
        next += 1
      }
    }
    Action::Substract => {
      if next > 0 {
        next -= 1
      }
    }
  }

  next
}

fn get_selection_from_list(opts: &Vec<&str>) -> Option<usize> {
  let stdin = stdin();
  let mut stdout = stdout().into_raw_mode().unwrap();
  let mut selected = 0;
  let mut rv = None;

  write_list(&opts, selected);

  for c in stdin.keys() {
    match c.unwrap() {
      Key::Down => {
        selected = get_next_selected(selected, &opts, Action::Add);
      }
      Key::Up => {
        selected = get_next_selected(selected, &opts, Action::Substract);
      }
      Key::Char('q') => break,
      Key::Ctrl('c') => break,
      Key::Char('\n') => {
        rv = Some(selected);
        break;
      }
      _ => {}
    }
    write_list(&opts, selected);
    stdout.flush().unwrap();
  }

  clear_console();
  writeln!(stdout, "{}", termion::cursor::Show).unwrap();

  rv
}

fn main() {
  let opts = vec!["foo", "bar", "baz", "bam"];
  let selected = get_selection_from_list(&opts);

  match selected {
    Some(n) => println!("You selected: {}", opts[n]),
    None => println!("You didn't select any option"),
  }
}
