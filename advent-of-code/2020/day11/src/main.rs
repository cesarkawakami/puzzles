use std::cell::Ref;
use std::cell::RefCell;
use std::{
    convert::{TryFrom, TryInto},
    io::{self, BufRead, BufReader},
};

#[derive(Clone)]
struct Layout {
    matrix: Vec<Vec<char>>,
    rows: i32,
    cols: i32,
    visibility_cache: RefCell<Option<Vec<Vec<i32>>>>,
}

fn make_layout(matrix: Vec<Vec<char>>) -> Layout {
    Layout {
        rows: matrix.len().try_into().unwrap(),
        cols: matrix[0].len().try_into().unwrap(),
        matrix,
        visibility_cache: RefCell::new(None),
    }
}

impl Layout {
    fn at(&self, row: i32, col: i32) -> char {
        let row_usize: usize = row.try_into().unwrap();
        let col_usize: usize = col.try_into().unwrap();
        self.matrix[row_usize][col_usize]
    }

    fn is_occupied(&self, row: i32, col: i32) -> bool {
        (row >= 0 && row < self.rows) && (col >= 0 && col < self.cols) && self.at(row, col) == '#'
    }

    fn occupied_neighbor_count(&self, row: i32, col: i32) -> i32 {
        const NEIGHBORS: [(i32, i32); 8] = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ];

        NEIGHBORS.iter().fold(0, |acc, &(dr, dc)| {
            acc + i32::try_from(self.is_occupied(row + dr, col + dc)).unwrap()
        })
    }

    fn generate_points(
        &self,
        (from_r, from_c): (i32, i32),
        (delta_r, delta_c): (i32, i32),
    ) -> impl Iterator<Item = (i32, i32)> + '_ {
        let rs = std::iter::successors(Some(from_r), move |&r| Some(r + delta_r));
        let cs = std::iter::successors(Some(from_c), move |&c| Some(c + delta_c));
        rs.zip(cs)
            .take_while(move |&(r, c)| 0 <= r && r < self.rows && 0 <= c && c < self.cols)
    }

    fn cell_next_state(&self, row: i32, col: i32) -> char {
        match self.at(row, col) {
            '.' => '.',
            'L' => {
                if self.occupied_neighbor_count(row, col) == 0 {
                    '#'
                } else {
                    'L'
                }
            }
            '#' => {
                if self.occupied_neighbor_count(row, col) >= 4 {
                    'L'
                } else {
                    '#'
                }
            }
            _ => panic!("unexpected cell type: {}", self.at(row, col)),
        }
    }

    fn next_state_generic<F: Fn(i32, i32) -> char>(&self, next_state_fn: F) -> Layout {
        let next_matrix: Vec<Vec<_>> = (0..self.rows)
            .map(|row| (0..self.cols).map(|col| next_state_fn(row, col)).collect())
            .collect();
        make_layout(next_matrix)
    }

    fn compute_visibility(&self) -> Vec<Vec<i32>> {
        let mut visibility: Vec<Vec<i32>> = (0..self.rows)
            .map(|_| (0..self.cols).map(|_| 0).collect())
            .collect();

        let lines: Vec<((i32, i32), (i32, i32), (i32, i32))> = vec![
            // root, normal, direction
            ((0, 0), (0, 1), (1, 0)),
            ((0, 0), (1, 0), (0, 1)),
            ((self.rows - 1, self.cols - 1), (0, -1), (-1, 0)),
            ((self.rows - 1, self.cols - 1), (-1, 0), (0, -1)),
            ((0, 0), (0, 1), (1, 1)),
            ((1, 0), (1, 0), (1, 1)),
            ((0, self.cols - 1), (0, -1), (1, -1)),
            ((1, self.cols - 1), (1, 0), (1, -1)),
            ((self.rows - 1, 0), (-1, 0), (-1, 1)),
            ((self.rows - 1, 1), (0, 1), (-1, 1)),
            ((self.rows - 1, self.cols - 1), (0, -1), (-1, -1)),
            ((self.rows - 2, self.cols - 1), (-1, 0), (-1, -1)),
        ];

        for (root, normal, dir) in lines {
            for start in self.generate_points(root, normal) {
                let mut last_seen_is_occupied = false;
                for (r, c) in self.generate_points(start, dir) {
                    if last_seen_is_occupied {
                        visibility[usize::try_from(r).unwrap()][usize::try_from(c).unwrap()] += 1;
                    }
                    let state = self.at(r, c);
                    if state == '#' {
                        last_seen_is_occupied = true;
                    } else if state == 'L' {
                        last_seen_is_occupied = false;
                    }
                }
            }
        }

        visibility
    }

    fn get_visibility(&self) -> Ref<Vec<Vec<i32>>> {
        if self.visibility_cache.borrow().is_none() {
            *self.visibility_cache.borrow_mut() = Some(self.compute_visibility());
        }
        Ref::map(self.visibility_cache.borrow(), |opt| opt.as_ref().unwrap())
    }

    fn cell_next_state_part2(&self, row: i32, col: i32) -> char {
        let visible_occupied =
            self.get_visibility()[usize::try_from(row).unwrap()][usize::try_from(col).unwrap()];
        match self.at(row, col) {
            '.' => '.',
            '#' => {
                if visible_occupied >= 5 {
                    'L'
                } else {
                    '#'
                }
            }
            'L' => {
                if visible_occupied == 0 {
                    '#'
                } else {
                    'L'
                }
            }
            _ => panic!("unexpected cell type: {}", self.at(row, col)),
        }
    }

    fn occupied_count(&self) -> i32 {
        self.matrix
            .iter()
            .flatten()
            .map(|&v| if v == '#' { 1 } else { 0 })
            .sum()
    }

    fn to_string(&self) -> String {
        self.matrix
            .iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl PartialEq for Layout {
    fn eq(&self, other: &Self) -> bool {
        self.rows == other.rows && self.cols == other.cols && self.matrix == other.matrix
    }
}

fn iterate_until_stable<T: PartialEq, F: Fn(&T) -> T>(start: T, next_fn: F) -> (i32, T) {
    let mut current = start;
    let mut count = 0;
    loop {
        let next = next_fn(&current);
        let should_break = next == current;
        current = next;
        count += 1;
        if should_break {
            break;
        }
    }
    (count, current)
}

fn main() {
    let reader = BufReader::new(io::stdin());
    let starting_layout: Vec<Vec<char>> = reader
        .lines()
        .flat_map(|line| line)
        .map(|line| line.trim().chars().collect())
        .collect();
    let starting_layout = make_layout(starting_layout);

    let (part1_count, part1_final_layout) =
        iterate_until_stable(starting_layout.clone(), |layout| {
            layout.next_state_generic(|r, c| layout.cell_next_state(r, c))
        });

    println!("part1, final layout:");
    println!("{}", part1_final_layout.to_string());
    println!(
        "part1, occupied count: {}",
        part1_final_layout.occupied_count()
    );
    println!("part1, transition count: {}", part1_count);

    let (part2_count, part2_final_layout) =
        iterate_until_stable(starting_layout.clone(), |layout| {
            layout.next_state_generic(|r, c| layout.cell_next_state_part2(r, c))
        });

    println!("part2, final layout:");
    println!("{}", part2_final_layout.to_string());
    println!(
        "part2, occupied count: {}",
        part2_final_layout.occupied_count()
    );
    println!("part2, transition count: {}", part2_count);
}
