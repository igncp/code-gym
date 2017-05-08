// list the entry types in the given path. e.g.
// cargo run -- ~/dev

fn main() {
    let original_path = match std::env::args().nth(1) {
        None => String::from(""),
        Some(path) => path,
    };

    if original_path.is_empty() {
        println!("You didn't pass a path");
        ::std::process::exit(1)
    }

    let entries = std::fs::read_dir(&original_path).unwrap();

    let mut entries_results = Vec::new();

    for r in entries {
        let entry = r.unwrap();
        let file_type = entry.file_type().unwrap();
        let full_path = String::from(format!("{}", entry.path().display()));
        let replacement_path = original_path.to_string() + "/";
        let relative_path = full_path.replace(&replacement_path, "");

        match file_type.is_dir() {
            true => entries_results.push(String::from("dir: ") + &relative_path),
            false => {
                match file_type.is_file() {
                    true => entries_results.push(String::from("file: ") + &relative_path),
                    false => entries_results.push(String::from("symlink: ") + &relative_path),
                }
            }
        }
    }

    entries_results.sort();

    for e in entries_results {
        println!("{}", e);
    }
}
