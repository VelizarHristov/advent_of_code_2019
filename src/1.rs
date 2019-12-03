use std::fs::File;
use std::io::{BufRead, BufReader};

fn fuel_for_module_mass(module_mass: i64) -> i64 {
    module_mass / 3 - 2
}

fn main() {
    let file = File::open("1.txt").unwrap();
    let lines = BufReader::new(file).lines();

    let total: i64 = lines.map(|line| {
        let num = line.unwrap().parse().unwrap();
        fuel_for_module_mass(num)
    }).sum();
    println!("{}", total);
}
