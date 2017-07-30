extern crate clap;

use clap::{App, SubCommand, Arg};

fn main() {
    let matches = App::new("command")
        .version("0.1.0")
        .about("This is the description")
        .arg(
            Arg::with_name("config")
                .short("c")
                .long("config")
                .value_name("FILE")
                .help("Sets a custom config file")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(Arg::with_name("v").short("v").multiple(true).help(
            "Sets the level of verbosity",
        ))
        .subcommand(
            SubCommand::with_name("foo")
                .about("does foo things")
                .arg_from_usage("-l, --list 'lists test values'"),
        )
        .get_matches();

    if let Some(o) = matches.value_of("output") {
        println!("Value for output: {}", o);
    }

    if let Some(c) = matches.value_of("config") {
        println!("Value for config: {}", c);
    }

    match matches.occurrences_of("d") {
        0 => println!("Debug mode is off"),
        1 => println!("Debug mode is kind of on"),
        2 => println!("Debug mode is on"),
        3 | _ => println!("Don't be crazy"),
    }

    if let Some(ref matches) = matches.subcommand_matches("foo") {
        if matches.is_present("list") {
            println!("Printing testing lists...");
        } else {
            println!("Not printing testing lists...");
        }
    }
}
