fn raii() {
    fn create_box() {
        let _box1 = Box::new("foo");

        // `_box1` is destroyed here, and memory gets freed
    }

    let _box2 = Box::new(10i32);

    // A nested scope:
    {
        let _box3 = Box::new(4.0);

        // `_box3` is destroyed here, and memory gets freed
    }

    for counter in 0..10 {
        create_box();
        println!("{}", counter);
    }

    // `_box2` is destroyed here, and memory gets freed
}

fn ownership() {
    // This function takes ownership of the heap allocated memory
    fn destroy_box(c: Box<i32>) {
        println!("Destroying a box that contains {}", c);

        // `c` is destroyed and the memory freed
    }

    // _Stack_ allocated integer
    let x = 5u32;

    // *Copy* `x` into `y` - no resources are moved
    let y = x;

    // Both values can be independently used
    println!("x is {}, and y is {}", x, y);

    // `a` is a pointer to a _heap_ allocated integer
    let a = Box::new(5i32);

    println!("a contains: {}", a);

    // *Move* `a` into `b`
    let b = a;
    // The pointer address of `a` is copied (not the data) into `b`.
    // Both are now pointers to the same heap allocated data, but
    // `b` now owns it.

    // Error! `a` can no longer access the data, because it no longer owns the
    // heap memory
    //println!("a contains: {}", a);
    // TODO ^ Try uncommenting this line

    // This function takes ownership of the heap allocated memory from `b`
    destroy_box(b);

    // Since the heap memory has been freed at this point, this action would
    // result in dereferencing freed memory, but it's forbidden by the compiler
    // Error! Same reason as the previous Error
    //println!("b contains: {}", b);
    // TODO ^ Try uncommenting this line
}

fn ownership_mutability() {
    let box_1 = Box::new(5);

    println!("box_1 contains {}", box_1);

    // Mutability error
    // *box_1 = 4;

    // *Move* the box, changing the ownership (and mutability)
    let mut mutable_box = box_1;

    println!("mutable_box contains {}", mutable_box);

    // Modify the contents of the box
    *mutable_box = 4;

    println!("mutable_box now contains {}", mutable_box);
}

fn main() {
    println!("starting raii");
    raii();
    println!("finished raii\n");

    println!("starting ownership");
    ownership();
    println!("finished ownership\n");

    println!("starting ownership mutability");
    ownership_mutability();
    println!("finished ownership mutability");
}
