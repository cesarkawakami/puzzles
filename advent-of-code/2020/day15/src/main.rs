use std::collections::HashMap;
use std::convert::TryInto;
use std::io::BufRead;
use std::io::BufReader;

struct State {
    numbers: Vec<i32>,
    lookup: HashMap<i32, i32>,
}

fn make_state(numbers: Vec<i32>) -> State {
    let lookup: HashMap<i32, i32> = numbers.iter().take(numbers.len() - 1).enumerate().fold(
        HashMap::new(),
        |mut prev_lookup, (turn, &number)| {
            prev_lookup.insert(number, turn.try_into().unwrap());
            prev_lookup
        },
    );

    State { numbers, lookup }
}

fn iterate(mut state: State) -> State {
    let last_spoken = *state.numbers.last().unwrap();
    let current_turn: i32 = state.numbers.len().try_into().unwrap();
    let next_spoken = match state.lookup.get(&last_spoken) {
        None => 0,
        Some(pos) => current_turn - pos - 1,
    };
    state.numbers.push(next_spoken);
    state.lookup.insert(last_spoken, current_turn - 1);
    state
}

fn iterate_n(state: State, n: i32) -> State {
    (0..n).fold(state, |last_state, _| iterate(last_state))
}

fn main() {
    let reader = BufReader::new(std::io::stdin());
    let initial_numbers: Vec<i32> = reader
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    let state = iterate_n(make_state(initial_numbers.clone()), 30_000_050);

    println!(
        "part1, seq: {}",
        state
            .numbers
            .iter()
            .take(10)
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    );
    println!("part1, 2020th: {}", state.numbers[2020 - 1]);

    println!("part2, 30_000_000th: {}", state.numbers[30_000_000 - 1]);
}
