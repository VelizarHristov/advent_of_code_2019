use std::fs::read_to_string;

fn str_to_direction(s: &str) -> (i32, i32) {
    let direction = s.chars().next().unwrap();
    let magnitude: i32 = s.to_string()[1..].parse().unwrap();
    if direction == 'L' {
        (-magnitude, 0)
    } else if direction == 'R' {
        (magnitude, 0)
    } else if direction == 'U' {
        (0, magnitude)
    } else {
        (0, -magnitude)
    }
}

fn get_positions(wire: &Vec<&str>) -> Vec<(i32, i32)> {
    wire.iter().scan((0, 0), |(x_pos, y_pos), &input_str| {
        let (x_change, y_change) = str_to_direction(input_str);
        *x_pos = *x_pos + x_change;
        *y_pos = *y_pos + y_change;
        Some((*x_pos, *y_pos))
    }).collect()
}

struct Line {
    start: (i32, i32),
    end: (i32, i32)
}

struct Line2 {
    start: (i32, i32),
    direction_is_x: bool,
    change: i32
}

fn get_lines(wire: &Vec<&str>) -> Vec<Line> {
    let start_line = Line2 { start: (0, 0), direction_is_x: true, change: 0 };
    let q = wire.iter().scan(start_line, |prev_line, &input_str| {
        let (x_pos, y_pos) = prev_line.start;
        let (x_change, y_change) = str_to_direction(input_str);
        *x_pos = *x_pos + x_change;
        *y_pos = *y_pos + y_change;
        // TODO: fix this code up
        Some(Line2 { start: (x_pos, y_pos), direction_is_x: true, change: 0 });
        Some((*x_pos, *y_pos))
    }).skip(1).collect();

    let positions = get_positions(wire);
    positions.iter().zip(positions.iter().skip(1)).map(|(p1, p2)|
        Line{start: *p1, end: *p2}
    ).collect()
}

fn lines_overlap_1d(x1_start: i32, x1_end: i32, x2_start: i32, x2_end: i32) -> bool {
    x1_start <= x2_end && x1_end >= x2_start
}

fn lines_min_intersection_1d(x1_start: i32, x1_end: i32, x2_start: i32, x2_end: i32) -> bool {
    let overlap = x1_start <= x2_end && x1_end >= x2_start;
    overlap
}

// TODO: instead of a boolean, return an optional of length?
fn lines_overlap(line_1: &Line, line_2: &Line) -> bool {
    let Line { start: (x1_start, y1_start), end: (x1_end, y1_end) } = line_1;
    let Line { start: (x2_start, y2_start), end: (x2_end, y2_end) } = line_2;
    lines_overlap_1d(*x1_start, *x1_end, *x2_start, *x2_end) &&
        lines_overlap_1d(*y1_start, *y1_end, *y2_start, *y2_end)
}

fn parse_str(s: &str) -> i64 {
    s.parse().unwrap()
}

fn main() {
    let file_contents = read_to_string("3.txt").unwrap();
    let data: Vec<Vec<&str>> = file_contents.split("\n").map(|s| s.split(",").collect()).collect();
    let wire_1: &Vec<&str> = &data[0];
    let wire_2: &Vec<&str> = &data[1];

    let wire_1_segments = get_lines(wire_1);
    let ans_todo_rename_me = get_lines(wire_2).iter().fold(-1, |outer_max, line_1| { // TODO: replace max with min
        wire_1_segments.iter().fold(outer_max, |max, line_2| {
            if lines_overlap(line_1, line_2) {
                let dist = line_1.start.0;
                dist
            } else {
                max
            }
        })
    });

    println!("{}", ans_todo_rename_me);
}
