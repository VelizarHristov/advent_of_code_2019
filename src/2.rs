use std::fs::read_to_string;

fn get_op(opcode: u64) -> fn(u64, u64) -> u64 {
    if opcode == 1 {
        |a, b| a + b
    } else {
        |a, b| a * b
    }
}

fn main() {
    let file_contents = read_to_string("2.txt").unwrap();
    let mut state: Vec<u64> = file_contents.split(",").map(|s| s.parse().unwrap()).collect();
    state[1] = 12; // as per spec
    state[2] = 2;  // as per spec
    let mut opcode_num = 0;
    loop {
        let opcode_index = opcode_num * 4;

        let opcode = state[opcode_index];
        if opcode == 99 {
            break;
        }
        let op = get_op(opcode);
        let input_1_idx = state[opcode_index + 1] as usize;
        let input_2_idx = state[opcode_index + 2] as usize;
        let output_idx = state[opcode_index + 3] as usize;
        let output = op(state[input_1_idx], state[input_2_idx]);

        state[output_idx] = output;
        opcode_num += 1;
    }
    println!("{}", state[0]);
}
