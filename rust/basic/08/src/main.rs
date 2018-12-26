// from: https://github.com/gifnksm/twitter-api-rs/blob/master/examples/tweet.rs

extern crate oauth_client as oauth;
extern crate serde_json;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate serde_derive;
extern crate serde;

mod twitter_api;

use oauth::Token;
use std::env;
use std::fs::{File, OpenOptions};
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::path::PathBuf;

const CREDENTIALS_FILENAME: &'static str = "credentials.json";

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
  pub consumer_key: String,
  pub consumer_secret: String,
  pub access_key: String,
  pub access_secret: String,
}

impl Config {
  pub fn read(path_file: &Path) -> Option<Config> {
    let mut file = match File::open(path_file) {
      Ok(f) => f,
      Err(_) => return None,
    };
    serde_json::from_reader(&mut file).ok()
  }

  pub fn write(&self, path_file: &Path) {
    let mut file = match OpenOptions::new().write(true).create(true).open(path_file) {
      Ok(f) => f,
      Err(e) => panic!("{}", e),
    };
    let _ = write!(&mut file, "{}\n", &serde_json::to_string(self).unwrap());
  }

  pub fn create(path_file: &Path) {
    match File::create(path_file) {
      Ok(_) => println!("File created!"),
      Err(_) => panic!("Problem to create the file...\nProgram aborted!"),
    }
  }
}

fn console_input(prompt: &str) -> String {
  println!("{} : ", prompt);
  let mut line = String::new();
  let _ = io::stdin().read_line(&mut line).unwrap();
  line.trim().to_string()
}

fn print_tweet_one_line(t: &twitter_api::Tweet) {
  println!(
    "{name} [{alias}]: {when} - {text}",
    name = t.user.name,
    alias = t.user.screen_name,
    when = t.created_at,
    text = t.text
  )
}

fn main() {
  let mut credentials_buffer: PathBuf = env::current_dir().unwrap();
  credentials_buffer.push(Path::new(CREDENTIALS_FILENAME));

  let conf = match Config::read(&credentials_buffer) {
    Some(c) => c,
    None => {
      Config::create(&credentials_buffer);

      let consumer_key = console_input("input your consumer key:");
      let consumer_secret = console_input("input your consumer secret:");
      let consumer = Token::new(consumer_key, consumer_secret);

      let request = twitter_api::get_request_token(&consumer).unwrap();
      println!("open the following url:");
      println!("\t{}", twitter_api::get_authorize_url(&request));
      let pin = console_input("input pin:");
      let access = twitter_api::get_access_token(&consumer, &request, &pin).unwrap();

      let c = Config {
        consumer_key: consumer.key.to_string(),
        consumer_secret: consumer.secret.to_string(),
        access_key: access.key.to_string(),
        access_secret: access.secret.to_string(),
      };

      c.write(&credentials_buffer);
      c
    }
  };

  let consumer = Token::new(conf.consumer_key, conf.consumer_secret);
  let access = Token::new(conf.access_key, conf.access_secret);

  // twitter_api::print_tweets_json(&consumer, &access);

  let tweets = twitter_api::get_last_tweets(&consumer, &access).unwrap();
  let tweet = &tweets[0];

  print_tweet_one_line(tweet);
}
