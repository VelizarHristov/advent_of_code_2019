fn matches_criteria(i: &i32) -> bool {
    let string = i.to_string();
    let adjacent_digit_pairs = string.chars().zip(string.chars().skip(1));
    let has_matching_adjacent_digits = adjacent_digit_pairs.clone().any(|(a, b)| a == b);
    let is_always_increasing = adjacent_digit_pairs.clone().all(|(a, b)| {
        a.to_digit(10) <= b.to_digit(10)
    });
    return has_matching_adjacent_digits && is_always_increasing;
}

fn main() {
    let len = (256310..732737).filter(matches_criteria).collect::<Vec<_>>().len();
    println!("{}", len);
}
