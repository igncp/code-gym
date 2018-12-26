// Playing around with manipulating JSON received from an API

extern crate hyper;
extern crate serde_json;

use hyper::*;
use serde_json::Value;
use std::io::Read;

static URL: &'static str = "http://jsonplaceholder.typicode.com/posts/1";

fn get_title(obj: &Value) -> String {
  return format!("{}", obj["title"]);
}

fn print_keys(obj: &Value) {
  println!("\nkeys:");
  for key in obj.as_object().unwrap().keys() {
    println!("key: {}", key);
  }
}

fn main() {
  let client = Client::new();
  let mut res = client.get(URL).send().unwrap();
  assert_eq!(res.status, hyper::Ok);
  let mut s = String::new();
  res.read_to_string(&mut s).unwrap();
  let v: Value = serde_json::from_str(&s).unwrap();
  let title = get_title(&v);

  println!("title: {}", title);

  print_keys(&v);
}
