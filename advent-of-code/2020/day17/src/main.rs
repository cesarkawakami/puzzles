use std::collections::HashSet;
use std::convert::TryInto;
use std::io::BufRead;
use std::io::BufReader;

struct State {
    active_cells: HashSet<(i32, i32, i32, i32)>,
    with_fourth: bool,
}

impl State {
    fn candidates(&self) -> HashSet<(i32, i32, i32, i32)> {
        self.active_cells
            .iter()
            .flat_map(|&p| neighbors(p, true, self.with_fourth))
            .collect()
    }

    fn is_active(&self, p: (i32, i32, i32, i32)) -> bool {
        self.active_cells.contains(&p)
    }

    fn is_active_in_next_state(&self, p: (i32, i32, i32, i32)) -> bool {
        let active_neighbors = neighbors(p, false, self.with_fourth)
            .filter(|&p| self.is_active(p))
            .count();
        if self.is_active(p) {
            2 <= active_neighbors && active_neighbors <= 3
        } else {
            active_neighbors == 3
        }
    }

    fn next_state(&self) -> State {
        let next_active_cells: HashSet<(i32, i32, i32, i32)> = self
            .candidates()
            .into_iter()
            .filter(|&p| self.is_active_in_next_state(p))
            .collect();
        State {
            active_cells: next_active_cells,
            with_fourth: self.with_fourth,
        }
    }

    fn line_to_string(&self, y: i32, z: i32, w: i32) -> String {
        let (min_x, max_x) = self.minmax_f(&|&(x, _, _, _)| x);
        (min_x..=max_x)
            .map(|x| {
                if self.is_active((x, y, z, w)) {
                    '#'
                } else {
                    '.'
                }
            })
            .collect::<String>()
    }

    fn plane_to_string(&self, z: i32, w: i32) -> String {
        let (min_y, max_y) = self.minmax_f(&|&(_, y, _, _)| y);
        (min_y..=max_y)
            .rev()
            .map(|y| self.line_to_string(y, z, w))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn minmax_f<T: Ord, F: Fn(&(i32, i32, i32, i32)) -> T>(&self, f: &F) -> (T, T) {
        (
            self.active_cells.iter().map(f).min().unwrap(),
            self.active_cells.iter().map(f).max().unwrap(),
        )
    }

    fn active_count(&self) -> i32 {
        self.active_cells.len().try_into().unwrap()
    }
}

impl Clone for State {
    fn clone(&self) -> Self {
        State {
            active_cells: self.active_cells.clone(),
            with_fourth: self.with_fourth,
        }
    }
}

fn state_from_input(input: &Vec<String>, with_fourth: bool) -> State {
    let active_cells: HashSet<_> = input
        .into_iter()
        .rev()
        .enumerate()
        .flat_map(|(y, row)| {
            let y: i32 = y.try_into().unwrap();
            row.chars().enumerate().flat_map(move |(x, ch)| {
                let x: i32 = x.try_into().unwrap();
                match ch {
                    '#' => Some((x, y, 0, 0)),
                    '.' => None,
                    _ => panic!("unexpected char: {}", ch),
                }
            })
        })
        .collect();
    State {
        active_cells,
        with_fourth,
    }
}

fn neighbors(
    (cx, cy, cz, cw): (i32, i32, i32, i32),
    include_self: bool,
    with_fourth: bool,
) -> impl Iterator<Item = (i32, i32, i32, i32)> {
    let (min_w, max_w) = match with_fourth {
        false => (cw, cw),
        true => (cw - 1, cw + 1),
    };
    (cx - 1..=cx + 1).flat_map(move |x| {
        (cy - 1..=cy + 1).flat_map(move |y| {
            (cz - 1..=cz + 1).flat_map(move |z| {
                (min_w..=max_w).flat_map(move |w| {
                    if !include_self && (x, y, z, w) == (cx, cy, cz, cw) {
                        None
                    } else {
                        Some((x, y, z, w))
                    }
                })
            })
        })
    })
}

fn main() {
    let reader = BufReader::new(std::io::stdin());
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap().trim().to_string())
        .collect();
    let initial_state = state_from_input(&lines, false);

    println!("initial z=0:");
    println!("{}", initial_state.plane_to_string(0, 0));

    let mut current_state = initial_state.clone();
    for iteration in 1..=6 {
        current_state = current_state.next_state();
        println!("after {}-th iteration z=0:", iteration);
        println!("{}", current_state.plane_to_string(0, 0));
    }

    println!("part1, count: {}", current_state.active_count());

    let initial_state_2 = state_from_input(&lines, true);
    println!("part2, initial (z, w) = (0, 0):");
    println!("{}", initial_state_2.plane_to_string(0, 0));

    let final_state_2 = (1..=6).fold(initial_state_2, |state, iteration| {
        let new_state = state.next_state();
        println!("part2, after {}-th iteration (z, w) = (0, 0)", iteration);
        println!("{}", new_state.plane_to_string(0, 0));
        new_state
    });
    println!("part2, count: {}", final_state_2.active_count())
}
