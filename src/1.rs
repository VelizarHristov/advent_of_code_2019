use std::fs::File;
use std::io::{BufRead, BufReader};

fn fuel_for_module_mass(module_mass: i64) -> i64 {
    module_mass / 3 - 2
}

fn main() {
    let file = File::open("1.txt").expect("Unable to open file");
    let br = BufReader::new(file);
    let data: Vec<i64> = br.lines()
        .map(|line| {
            let str = line.expect("Could not parse line");
            let num = str.parse::<i64>().unwrap();
            fuel_for_module_mass(num)
        })
        .collect();

    let total: i64 = data.iter().sum();
    println!("{}", total);
}
