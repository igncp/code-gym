extern crate jsonwebtoken as jwt;
extern crate progress;
#[macro_use]
extern crate serde_derive;

use jwt::errors::ErrorKind;
use jwt::{decode, encode, Header, Validation};
use std::io::{stdin, stdout, Write};
use std::{env, thread, time};

fn add_progress_effect(message: &str) {
  let mut bar = progress::Bar::new();

  bar.set_job_title(message);

  for i in 0..11 {
    let dur = time::Duration::from_millis(50);
    thread::sleep(dur);
    bar.reach_percent(i * 10);
  }
}

fn ask_for_value(message: String) -> String {
  let mut s = String::new();

  print!("{}: ", message);

  let _ = stdout().flush();

  stdin()
    .read_line(&mut s)
    .expect("Did not enter a correct string");

  if let Some('\n') = s.chars().next_back() {
    s.pop();
  }

  if let Some('\r') = s.chars().next_back() {
    s.pop();
  }

  s
}

#[derive(Debug, Serialize, Deserialize)]
struct Claims {
  name: String,
  iss: String,
}

fn serialize() -> String {
  let name = ask_for_value("Please, enter the name".to_owned());
  let key = ask_for_value("Please, enter a secret".to_owned());
  let my_claims = Claims {
    name: name,
    iss: "this-script".to_string(),
  };

  add_progress_effect("Serializing...");

  let token = match encode(&Header::default(), &my_claims, key.as_ref()) {
    Ok(t) => t,
    Err(_) => panic!(),
  };

  token
}

fn deserialize() -> jwt::TokenData<Claims> {
  let token = ask_for_value("Please, enter the token".to_owned());
  let key = ask_for_value("Please, enter the secret".to_owned());

  let validation = Validation {
    iss: Some("this-script".to_string()),
    ..Validation::default()
  };

  add_progress_effect("Deserializing...");

  let token_data = match decode::<Claims>(&token, key.as_ref(), &validation) {
    Ok(c) => c,
    Err(err) => match *err.kind() {
      ErrorKind::InvalidToken => panic!("Invalid token"),
      ErrorKind::InvalidIssuer => panic!("Invalid issuer"),
      ErrorKind::InvalidSignature => panic!("Invalid signature"),
      _ => panic!("Unknown error: {}", err.kind()),
    },
  };

  token_data
}

fn main() {
  let action = env::args()
    .nth(1)
    .expect("You need to pass serialize or deserialize");

  if action == "serialize" {
    let result = serialize();

    println!("Result: {}", result)
  } else if action == "deserialize" {
    let result = deserialize();

    println!("{:?}", result.claims);
    println!("{:?}", result.header);
  }
}
