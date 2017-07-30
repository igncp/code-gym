extern crate walkdir;
extern crate regex;

use std::env;
use std::io::prelude::*;
use std::fs::File;

use regex::Regex;
use walkdir::WalkDir;

fn main() {
    let args: Vec<String> = env::args().collect();
    let dir = args.get(1).unwrap();
    let search_str = args.get(2).unwrap();
    let re = Regex::new(search_str).unwrap();
    let lines_re = Regex::new("\n").unwrap();

    println!("Searching in {}", dir);

    for entry in WalkDir::new(dir) {
        let entry = match entry {
            Ok(v) => v,
            Err(_) => continue,
        };

        let path_display = entry.path().display().to_string();
        let is_file = entry.file_type().is_file();

        if is_file == true {
            let mut file = File::open(&path_display).expect("Unable to open the file");
            let mut contents = String::new();

            let result = file.read_to_string(&mut contents);

            match result {
                Err(_) => continue,
                Ok(_) => (),
            }

            if re.is_match(&contents) {
                let mut lines: Vec<usize> = Vec::new();

                for mat in lines_re.find_iter(&contents) {
                    lines.push(mat.start());
                }

                for mat in re.find_iter(&contents) {
                    let start = mat.start();

                    for (i, line) in lines.iter().enumerate() {
                        if &start < line {
                            println!("{}: {}", path_display, i + 1);

                            break;
                        }
                    }
                }
            }
        }
    }
}
