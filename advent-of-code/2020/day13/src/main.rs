use std::convert::TryFrom;
use std::io::BufRead;
use std::io::BufReader;

type Num = i128;

fn extended_gcd(a: Num, b: Num) -> (Num, Num, Num) {
    let mut rs = (a, b);
    let mut ss = (1 as Num, 0 as Num);
    let mut ts = (0 as Num, 1 as Num);
    while rs.1 != 0 {
        let quotient = rs.0 / rs.1;
        rs = (rs.1, rs.0 - quotient * rs.1);
        ss = (ss.1, ss.0 - quotient * ss.1);
        ts = (ts.1, ts.0 - quotient * ts.1);
    }
    (rs.0, ss.0, ts.0)
}

fn solve_modular_pair(a: Num, m: Num, b: Num, n: Num) -> Option<(Num, Num)> {
    let (g, u, v) = extended_gcd(m, n);
    let lcm = m / g * n;
    let solution = n / g * a % lcm * v % lcm + m / g * b % lcm * u % lcm;
    if a % g == b % g {
        Some((math_mod(solution, lcm), lcm))
    } else {
        None
    }
}

fn solve_modular_system(system: &[(Num, Num)]) -> Option<(Num, Num)> {
    if system.is_empty() {
        return None;
    }
    system.iter().fold(Some(system[0]), |res, &(a, m)| {
        res.and_then(|(b, n)| solve_modular_pair(a, m, b, n))
    })
}

fn math_mod(a: Num, m: Num) -> Num {
    (a % m + m) % m
}

fn modular_system_from_bus_ids(bus_ids: &[Option<i32>]) -> Vec<(Num, Num)> {
    bus_ids
        .iter()
        .enumerate()
        .filter_map(|(i, &bus_id)| match bus_id {
            Some(bus_id) => Some((Num::try_from(i).unwrap(), Num::try_from(bus_id).unwrap())),
            None => None,
        })
        .map(|(i, bus_id)| (math_mod(-i, bus_id), bus_id))
        .collect()
}

fn main() {
    let mut lines_iter = BufReader::new(std::io::stdin()).lines();
    let start_time: i32 = lines_iter.next().unwrap().unwrap().parse().unwrap();
    let bus_ids: Vec<Option<i32>> = lines_iter
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|raw_bus| match raw_bus {
            "x" => None,
            raw_bus => Some(raw_bus.parse().unwrap()),
        })
        .collect();

    let in_service_bus_ids: Vec<i32> = bus_ids.iter().filter_map(|&x| x).collect();
    let next_timestamp_for_each_bus: Vec<i32> = in_service_bus_ids
        .iter()
        .map(|bus_id| (start_time + bus_id - 1) / bus_id * bus_id)
        .collect();

    let (earliest_timestamp, earliest_bus_id) = next_timestamp_for_each_bus
        .iter()
        .zip(in_service_bus_ids.iter())
        .min()
        .unwrap();

    println!("part1, earliest timestamp: {}", earliest_timestamp);
    println!("part1, earliest bus_id:    {}", earliest_bus_id);
    println!(
        "part1, answer:             {}",
        earliest_bus_id * (earliest_timestamp - start_time)
    );

    let modular_system = modular_system_from_bus_ids(&bus_ids);
    let (mod_solution_a, mod_solution_m) = solve_modular_system(&modular_system).unwrap();
    println!("part2, a: {}, m: {}", mod_solution_a, mod_solution_m);
    // println!("part2, answer: {}", mod_solution_a + mod_solution_m);
}
