use std::{
    collections::HashSet,
    io::{self, BufRead, BufReader},
};

fn seat_to_coordinate(mut acc_row: i32, mut acc_col: i32, seat: &[char]) -> (i32, i32) {
    if seat.len() == 0 {
        return (acc_row, acc_col);
    }
    if ['F', 'B'].contains(&seat[0]) {
        acc_row = (acc_row << 1) + if seat[0] == 'F' { 0 } else { 1 };
    }
    if ['L', 'R'].contains(&seat[0]) {
        acc_col = (acc_col << 1) + if seat[0] == 'L' { 0 } else { 1 };
    }
    seat_to_coordinate(acc_row, acc_col, &seat[1..])
}

fn main() {
    let reader = BufReader::new(io::stdin());
    let passes: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap().trim().to_string())
        .collect();
    let coordinates: Vec<(i32, i32)> = passes
        .iter()
        .map(|pass| seat_to_coordinate(0, 0, &pass.chars().collect::<Vec<char>>()[..]))
        .collect();
    let seat_ids: Vec<i32> = coordinates.iter().map(|(r, c)| 8 * r + c).collect();
    let max_seat_id = *seat_ids.iter().max().expect("no elements");
    println!("part 1, max seat id: {}", max_seat_id);
    let min_seat_id = *seat_ids.iter().min().expect("no elements");
    let all_seats: HashSet<i32> = (min_seat_id..max_seat_id).collect();
    let unseen_seats: Vec<i32> = all_seats
        .difference(&seat_ids.iter().copied().collect())
        .copied()
        .collect();
    println!("part 2, unseen: {:?}", unseen_seats);
}
