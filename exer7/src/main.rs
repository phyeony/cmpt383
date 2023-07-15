
pub mod find;
pub mod hailstone;
pub mod rational;

use hailstone::{hailstone, hailstone_sequence_append, hailstone_sequence_prealloc};
use find::{find_elt};
use rational::Rational;

fn main() {
    // nothing is required here, but you may want to use it for testing.
    let n = 14;
    let hailstone_result = hailstone(n);
    println!("Hailstone result for {}: {}", n, hailstone_result);

    let n2 = 5;
    let sequence_append_result = hailstone_sequence_append(n2);
    println!("Hailstone sequence (append) for {}: {:?}", n2, sequence_append_result);

    let sequence_prealloc_result = hailstone_sequence_prealloc(n2);
    println!("Hailstone sequence (prealloc) for {}: {:?}", n2, sequence_prealloc_result);

    let v1: Vec<i32> = Vec::from([4, 5, 2, 8, 7, 3, 1]);
    println!("{:?}", find_elt(&v1, 8)); // Some(3)
    println!("{:?}", find_elt(&v1, 6)); // None
    let v2: Vec<char> = "Hello World!".chars().collect();
    println!("{:?}", find_elt(&v2, 'o')); // Some(4)
    println!("{:?}", find_elt(&v2, 'q')); // None

    let mut r = Rational::new(6, 8);
    println!("{:?}", r); // prints Rational { n: 6, d: 8 }
    r.reduce();
    println!("{:?}", r); // prints Rational { n: 3, d: 4 }
    let n = Rational::from(4_i64);
    println!("{:?}", n); // prints Rational { n: 4, d: 1 }
    println!("{}", n == Rational::new(4,1)); // prints true
}
