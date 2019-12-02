use std::fs::File;
use std::io::{BufRead, BufReader};
use std::cmp::max;

fn fuel_for_mass(mass: i64) -> i64 {
    max(mass / 3 - 2, 0)
}

fn fuel_for_fuel(fuel_mass: i64) -> i64 {
    if fuel_mass > 0 {
        let extra_fuel = fuel_for_mass(fuel_mass);
        extra_fuel + fuel_for_fuel(extra_fuel)
    } else {
        0
    }
}

fn total_fuel_for_module(module_mass: i64) -> i64 {
    let fuel_mass = fuel_for_mass(module_mass);
    fuel_mass + fuel_for_fuel(fuel_mass)
}

fn main() {
    let file = File::open("1.txt").expect("Unable to open file");
    let br = BufReader::new(file);
    let data: Vec<i64> = br.lines()
        .map(|line| {
            let str = line.expect("Could not parse line");
            let num = str.parse::<i64>().unwrap();
            total_fuel_for_module(num)
        })
        .collect();

    let total: i64 = data.iter().sum();
    println!("{}", total);
}
