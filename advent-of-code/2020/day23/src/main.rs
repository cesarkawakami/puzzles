use std::convert::TryFrom;
use std::convert::TryInto;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::once;
use std::iter::successors;

#[derive(Clone, Debug)]
struct Linked {
    next: Vec<i32>,
    last: i32,
}

impl Linked {
    fn take_one(&mut self) -> i32 {
        let first: i32 = self.next[0];
        let second: i32 = self.next[usize::try_from(first).unwrap()];
        let third: i32 = self.next[usize::try_from(second).unwrap()];
        assert!(second != self.last);
        assert!(third != self.last);
        self.next[usize::try_from(first).unwrap()] = third;
        self.next[usize::try_from(second).unwrap()] = -1;
        second
    }

    fn take_n(&mut self, n: i32) -> Vec<i32> {
        (0..n).map(|_| self.take_one()).collect()
    }

    fn destination_cup(&self, taken: &[i32]) -> i32 {
        let current_cup = self.next[0];
        let mut destination_cup = current_cup;
        loop {
            destination_cup -= 1;
            if destination_cup < 1 {
                destination_cup = i32::try_from(self.next.len()).unwrap() - 1;
            }
            assert!(destination_cup != current_cup);

            if taken.iter().find(|&&v| v == destination_cup).is_none() {
                break destination_cup;
            }
        }
    }

    fn insert(&mut self, after: i32, value: i32) {
        assert!(self.next[usize::try_from(value).unwrap()] == -1);
        let next = self.next[usize::try_from(after).unwrap()];
        self.next[usize::try_from(after).unwrap()] = value;
        self.next[usize::try_from(value).unwrap()] = next;
        if after == self.last {
            self.last = value;
        }
    }

    fn insert_all(&mut self, after: i32, values: &[i32]) {
        values.iter().for_each(|&v| self.insert(after, v));
    }

    fn rotate(&mut self) {
        let first = self.next[0];
        let second = self.next[usize::try_from(first).unwrap()];
        self.next[usize::try_from(first).unwrap()] = -1;
        self.next[0] = second;
        self.insert(self.last, first);
    }

    fn do_move(&mut self) {
        let mut taken = self.take_n(3);
        taken.reverse();
        // println!("after taken: {}", self.to_string());
        let destination_cup = self.destination_cup(&taken);
        // println!("dest: {}", destination_cup);
        self.insert_all(destination_cup, &taken);
        // println!("after insert: {}", self.to_string());
        self.rotate();
        // println!("after rotate: {}", self.to_string());
    }

    fn iter(&self) -> impl Iterator<Item = i32> + Clone + '_ {
        successors(Some(self.next[0]), move |&v| {
            let next = self.next[usize::try_from(v).unwrap()];
            if next == -1 {
                None
            } else {
                Some(next)
            }
        })
    }

    fn get_output(&self) -> String {
        self.iter()
            .cycle()
            .skip_while(|&v| v != 1)
            .skip(1)
            .take(self.next.len() - 2)
            .map(|v| v.to_string())
            .collect()
    }

    fn to_string(&self) -> String {
        self.iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn part2_answer(&self) -> i64 {
        let elements = self
            .iter()
            .skip_while(|&v| v != 1)
            .skip(1)
            .take(2)
            .collect::<Vec<_>>();
        match elements.as_slice() {
            &[left, right] => {
                let left: i64 = left.into();
                let right: i64 = right.into();
                left * right
            }
            _ => panic!(),
        }
    }
}

fn make_linked(n: i32, source: &[i32]) -> Linked {
    let source_len = i32::try_from(source.len()).unwrap();
    assert!(n >= source_len);
    let mut next: Vec<i32> = (1..=n).chain(once(-1)).collect();

    {
        let left_iter = once(0).chain(source.iter().copied());
        let right_iter = source.iter().copied().chain(once(source_len + 1));
        left_iter.zip(right_iter).for_each(|(l, r)| {
            next[usize::try_from(l).unwrap()] = r;
        });
    }

    let mut last = n;
    if n == source_len {
        last = *source.last().unwrap();
    }
    next[usize::try_from(last).unwrap()] = -1;

    Linked { next, last }
}

fn parse_input(line: &str) -> Vec<i32> {
    line.chars()
        .map(|c| c.to_digit(10).unwrap().try_into().unwrap())
        .collect()
}

fn main() {
    let input = BufReader::new(std::io::stdin())
        .lines()
        .next()
        .unwrap()
        .unwrap()
        .trim()
        .to_string();
    let input_numbers = parse_input(input.as_str());
    let part1_linked = make_linked(9, &input_numbers);

    println!("part1, initial: {}", part1_linked.to_string());

    let part1_linked_after_10_moves = {
        let mut linked = part1_linked.clone();
        (1..=10).for_each(|_move_number| {
            linked.do_move();
            // println!("part1, after move {}: {}", move_number, linked.to_string());
        });
        linked
    };
    println!(
        "answer after 10 moves: {}",
        part1_linked_after_10_moves.get_output(),
    );
    let part1_linked_after_100_moves = {
        let mut linked = part1_linked.clone();
        (0..100).for_each(|_| linked.do_move());
        linked
    };
    println!(
        "part1, answer after 100 moves: {}",
        part1_linked_after_100_moves.get_output(),
    );

    let part2_linked = make_linked(1_000_000, &input_numbers);
    // let part2_linked = make_linked(20, &input_numbers);
    // println!("part2 debug: {:?}", part2_linked);
    // println!(
    //     "part2, debug: {}",
    //     part2_linked
    //         .iter()
    //         .take(15)
    //         .map(|v| v.to_string())
    //         .collect::<Vec<_>>()
    //         .join(" ")
    // );
    let part2_linked_post_moves = {
        let mut linked = part2_linked.clone();
        (0..10_000_000).for_each(|_| linked.do_move());
        linked
    };
    // println!(
    //     "part2, debug: {}",
    //     part2_linked_post_moves
    //         .iter()
    //         .take(15)
    //         .map(|v| v.to_string())
    //         .collect::<Vec<_>>()
    //         .join(" ")
    // );
    println!("part2, answer: {}", part2_linked_post_moves.part2_answer());
}
