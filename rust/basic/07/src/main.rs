// http://rustbyexample.com/trait.html

use std::time::Duration;
use std::thread;

trait Animal {
    fn new(name: &'static str) -> Self;

    fn grow(&mut self);

    fn name(&self) -> &'static str;

    // Traits can provide default method definitions.
    fn communicate(&self) {
        println!("{} says mmm", self.name());
    }
}

struct Dog {
    is_big: bool,
    name: &'static str,
}

impl Dog {
    fn get_is_big(&self) -> bool {
        self.is_big
    }

    fn bharf(&self) {
        println!("bharf!");
    }
}

impl Animal for Dog {
    fn new(name: &'static str) -> Dog {
        Dog {
            name: name,
            is_big: false,
        }
    }

    fn name(&self) -> &'static str {
        self.name
    }

    // Default trait methods can be overridden.
    fn communicate(&self) {
        println!("{} pauses briefly...", self.name);
        self.bharf();
    }

    fn grow(&mut self) {
        self.is_big = true;
        println!("{} is big now!", self.name)
    }
}

fn main() {
    // Type annotation is necessary in this case.
    let mut jim: Dog = Animal::new("Jim");

    let is_big = jim.get_is_big();

    println!("is {} big? {}", jim.name(), is_big);

    jim.communicate();

    jim.grow();

    loop {
        thread::sleep(Duration::from_millis(1000));

        jim.communicate();
    }
}
