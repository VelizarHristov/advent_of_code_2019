fn has_group_of_two(string: &String) -> bool {
    let first_char = string.chars().next().unwrap();
    let sz = string.chars().take_while(|c| *c == first_char).count();
    let remaining: &String = &string.chars().skip(sz).collect();
    sz == 2 || (!remaining.is_empty() && has_group_of_two(remaining))
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
