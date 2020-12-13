use std::io;
use std::io::BufRead;
use std::io::BufReader;
use std::collections::HashSet;
use std::time::Instant;

const TARGET: i32 = 2020;

fn main() {
    let start = Instant::now();
    let reader = BufReader::new(io::stdin());
    let numbers: Vec<i32> = reader.lines().map(|line| line.unwrap().parse().unwrap()).collect();

    let past_input = Instant::now();

    let mut previous_numbers = HashSet::new();

    for x in numbers.iter() {
        if previous_numbers.contains(&(TARGET - x)) {
            println!("{} * {} = {}", x, TARGET - x, x * (TARGET - x));
        }
        previous_numbers.insert(x);
    }

    for x in numbers.iter() {
        for y in numbers.iter() {
            if previous_numbers.contains(&(TARGET - x - y)) {
                println!("{} * {} * {} = {}", x, y, TARGET - x - y, x * y * (TARGET - x - y));
            }
        }
    }

    println!("whole: {}", start.elapsed().as_micros());
    println!("proc: {}", past_input.elapsed().as_micros());
}
