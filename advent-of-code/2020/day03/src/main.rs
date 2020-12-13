use std::convert::TryInto;
use std::io::{self, BufRead, BufReader};

fn count_trees(map: &Vec<Vec<bool>>, delta_row: usize, delta_col: usize) -> i64 {
    let mut row = delta_row;
    let mut col = delta_col;
    let mut tree_count = 0_i64;

    while row < map.len().try_into().expect("huh") {
        if map[row][col] {
            tree_count += 1;
        }
        row += delta_row;
        col = (col + delta_col) % map[0].len();
    }

    tree_count
}

fn main() {
    let reader = BufReader::new(io::stdin());
    let map: Vec<Vec<bool>> = reader
        .lines()
        .map(|line| {
            line.expect("failed to read")
                .trim()
                .chars()
                .map(|ch| match ch {
                    '.' => false,
                    '#' => true,
                    _ => panic!("invalid character"),
                })
                .collect()
        })
        .collect();

    {
        let tree_count = count_trees(&map, 1, 3);
        println!("part1, tree count: {}", tree_count);
    }

    {
        let cases = &[(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)];
        let mut result = 1_i64;
        for (delta_row, delta_col) in cases.iter() {
            let tree_count = count_trees(&map, *delta_row, *delta_col);
            result *= tree_count;
            println!("part2, tree count: {}", tree_count);
        }
        println!("part2, result: {}", result);
    }
}
