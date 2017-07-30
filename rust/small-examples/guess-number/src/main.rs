extern crate rand;

use std::io::{stdin, stdout, Write};

use rand::Rng;

fn get_line_without_line_break(s: String) -> String {
    let mut line_without_line_break = s;

    if let Some('\n') = line_without_line_break.chars().next_back() {
        line_without_line_break.pop();
    }

    if let Some('\r') = line_without_line_break.chars().next_back() {
        line_without_line_break.pop();
    }

    line_without_line_break
}

fn main() {
    let rand_num: u32 = rand::thread_rng().gen_range(0, 10);
    let mut user_points: u32 = 20;
    let mut tried_nums: Vec<u32> = Vec::new();

    loop {
        println!("You have {} points", user_points);

        print!("Please enter a number between 0 and 10: ");
        let mut s = String::new();
        let _ = stdout().flush();

        stdin().read_line(&mut s).expect(
            "You did not enter a correct string",
        );

        let line_without_line_break: String = get_line_without_line_break(s);
        let parsed_num = match line_without_line_break.parse::<u32>() {
            Ok(v) => v,
            Err(_) => continue,
        };

        if tried_nums.contains(&parsed_num) {
            println!("You already tried that number");
            continue;
        }

        tried_nums.push(parsed_num);

        if parsed_num == rand_num {
            println!("You won! You have {} points", user_points);
            break;
        } else {
            user_points -= 1;
        }

        if user_points == 0 {
            println!("You lost");
            break;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn get_line_without_line_break_test() {
        let s_1 = "Foo";
        let s_2 = "Bar\n";
        let s_3 = "Foo\n\r";
        let s_4 = "Foo\r\n";

        assert!(get_line_without_line_break(s_1.to_string()) == s_1);
        assert!(get_line_without_line_break(s_2.to_string()) == "Bar");
        assert!(get_line_without_line_break(s_3.to_string()) == "Foo\n");
        assert!(get_line_without_line_break(s_4.to_string()) == "Foo");
    }
}
