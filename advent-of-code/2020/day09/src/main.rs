use std::{
    collections::VecDeque,
    io::{self, BufRead, BufReader},
};

#[derive(Debug)]
struct NumberBag {
    memory: VecDeque<i32>,
    limit: usize,
}

impl NumberBag {
    fn add(&mut self, x: i32) -> () {
        self.memory.push_back(x);
        while self.memory.len() > self.limit {
            self.memory.pop_front();
        }
    }

    fn possible_pairs(&self) -> impl Iterator<Item = (i32, i32)> + '_ {
        self.memory.iter().flat_map(move |&x1| {
            self.memory
                .iter()
                .filter_map(move |&x2| if x1 == x2 { None } else { Some((x1, x2)) })
        })
    }

    fn possible_sums(&self) -> impl Iterator<Item = i32> + '_ {
        self.possible_pairs().map(|(x1, x2)| x1 + x2)
    }
}

fn main() {
    let reader = BufReader::new(io::stdin());
    let numbers: Vec<i32> = reader
        .lines()
        .flat_map(|line| line)
        .flat_map(|line| line.trim().parse())
        .collect();

    let &part2_target = {
        // const LIMIT: usize = 5;
        const LIMIT: usize = 25;
        let mut bag = NumberBag {
            memory: VecDeque::new(),
            limit: LIMIT,
        };
        let (first_non_sum_idx, first_non_sum) = numbers
            .iter()
            .enumerate()
            .find(|(i, &x)| {
                // println!("debug: {:?}", bag.possible_sums().collect::<Vec<_>>());
                let found = bag.possible_sums().find(|&s| x == s).is_none();
                bag.add(x);
                if i < &LIMIT {
                    false
                } else {
                    found
                }
            })
            .unwrap();
        println!("part1, idx:{} n:{}", first_non_sum_idx, first_non_sum);
        first_non_sum
    };

    {
        let mut left = 0_usize;
        let mut right = 0_usize;
        let mut sum = 0;

        while right < numbers.len() {
            sum += numbers[right];
            right += 1;
            while right < numbers.len() && sum < part2_target {
                sum += numbers[right];
                right += 1;
            }
            while left < numbers.len() && sum > part2_target {
                sum -= numbers[left];
                left += 1;
            }
            if sum == part2_target {
                break;
            }
        }
        assert!(sum == part2_target);

        let number_slice = &numbers[left..right];
        let smallest = number_slice.iter().min().unwrap();
        let largest = number_slice.iter().max().unwrap();
        println!(
            "part2, left:{} right:{} sum:{} smallest:{} largest:{} answer:{}",
            left,
            right,
            number_slice.iter().sum::<i32>(),
            smallest,
            largest,
            smallest + largest
        );
    }
}
