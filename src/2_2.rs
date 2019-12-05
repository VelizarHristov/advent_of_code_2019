use std::fs::read_to_string;

fn get_op(opcode: u64) -> fn(u64, u64) -> u64 {
    if opcode == 1 {
        |a, b| a + b
    } else {
        |a, b| a * b
    }
}

// TODO: check whether using recursion instead of mutable state is going to be nicer
fn main() {
    let file_contents = read_to_string("2.txt").unwrap();
    for program_input_1 in 0..100 {
        for program_input_2 in 0..100 {
            let mut state: Vec<u64> = file_contents.split(",").map(|s| s.parse().unwrap()).collect();
            state[1] = program_input_1;
            state[2] = program_input_2;
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
            if state[0] == 19690720 {
                println!("Answer: {}", 100 * program_input_1 + program_input_2);
                return;
            }
        }
    }
    println!("Error! No matches.");
}
