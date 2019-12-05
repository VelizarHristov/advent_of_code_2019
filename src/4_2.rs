fn has_group_of_two(string: &String) -> bool {
    if string.is_empty() {
        false
    } else {
        let first_char = string.chars().nth(0).unwrap();
        let subsequence_size = string.chars().take_while(|c|
            *c == first_char
        ).collect::<Vec<_>>().len();
        if subsequence_size == 2 {
            true
        } else {
            has_group_of_two(&string.chars().skip(subsequence_size).collect())
        }
    }
}

fn matches_criteria(i: &i32) -> bool {
    let string = i.to_string();
    let adjacent_digit_pairs = string.chars().zip(string.chars().skip(1));
    let is_always_increasing = adjacent_digit_pairs.clone().all(|(a, b)| {
        a.to_digit(10) <= b.to_digit(10)
    });
    return has_group_of_two(&string) && is_always_increasing;
}

fn main() {
    let len = (256310..732737).filter(matches_criteria).collect::<Vec<_>>().len();
    println!("{}", len);
}
