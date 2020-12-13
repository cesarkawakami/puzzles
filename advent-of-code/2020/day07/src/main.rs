use std::{
    collections::{HashMap, HashSet},
    io::{self, BufRead, BufReader},
};

fn iter_zipper<T>(it: &mut dyn Iterator<Item = T>) -> Option<(T, T)> {
    let (left, right) = (it.next(), it.next());
    return left.zip(right);
}

fn split_once<'a>(s: &'a str, pat: &str) -> Option<(&'a str, &'a str)> {
    iter_zipper(&mut s.splitn(2, pat))
}

fn rsplit_once<'a>(s: &'a str, pat: &str) -> Option<(&'a str, &'a str)> {
    iter_zipper(&mut s.rsplitn(2, pat))
}

fn parse_bag_description(bag_description_str: &str) -> Option<(&str, i32)> {
    if bag_description_str == "no other bags" {
        return None;
    }
    let (count_str, rest) = split_once(bag_description_str, " ").unwrap();
    let count: i32 = count_str
        .parse()
        .unwrap_or_else(|_| panic!("not int: {}, context: {}", count_str, bag_description_str));
    let (_, bag_type) = rsplit_once(rest, " bag").unwrap();
    // println!("desc:{} type:{} cnt:{}", bag_description_str, bag_type, count);
    Some((bag_type, count))
}

fn parse_bag_contents(bag_contents_str: &str) -> Vec<(&str, i32)> {
    let descs = bag_contents_str.split(", ");
    descs.map(parse_bag_description).flatten().collect()
}

fn parse_bag(bag_str: &str) -> (&str, Vec<(&str, i32)>) {
    let (bag_type, rest) = split_once(bag_str, " bags contain ").unwrap();
    let bag_contents = &rest[..rest.len() - 1];
    // println!("type:{} contents:{}", bag_type, bag_contents);
    (bag_type, parse_bag_contents(bag_contents))
}

fn traverse_all<'a>(edges: &HashMap<&str, Vec<&'a str>>, needle: &str) -> Vec<&'a str> {
    let mut result_set = HashSet::new();
    let mut to_visit = vec![needle];
    while !to_visit.is_empty() {
        let current = to_visit.pop().unwrap();
        if edges.contains_key(current) {
            for &container in &edges[current] {
                if !result_set.contains(container) {
                    result_set.insert(container);
                    to_visit.push(container);
                }
            }
        }
    }
    result_set.into_iter().collect()
}

fn count_contained<'a>(
    count_cache: &mut HashMap<&'a str, i32>,
    bags: &'a HashMap<&'a str, Vec<(&str, i32)>>,
    needle: &'a str,
) -> i32 {
    if !count_cache.contains_key(needle) {
        if !bags.contains_key(needle) {
            count_cache.insert(needle, 1);
        } else {
            let count = bags[needle].iter().fold(1, |acc, (sub_bag, sub_count)| {
                acc + sub_count * count_contained(count_cache, bags, sub_bag)
            });
            count_cache.insert(needle, count);
        }
    }
    count_cache[needle]
}

fn main() {
    let reader = BufReader::new(io::stdin());
    let bag_strs: Vec<String> = reader
        .lines()
        .map(|maybe_line| maybe_line.unwrap())
        .collect();
    let bags: HashMap<&str, Vec<(&str, i32)>> = bag_strs
        .iter()
        .map(|bag_str| parse_bag(&bag_str[..]))
        .collect();
    // println!("{:?}", bags);
    let containers = bags.iter().fold(
        HashMap::new(),
        |mut acc, (bag_type, contents)| {
            contents.iter().for_each(|(contained_bag_type, _count)| {
                acc.entry(*contained_bag_type)
                    .or_insert(Vec::new())
                    .push(*bag_type)
            });
            acc
        },
    );
    // println!("{:?}", containers);
    println!(
        "part1: {}",
        dbg!(traverse_all(&containers, &"shiny gold".to_string())).len()
    );
    println!(
        "part2: {}",
        count_contained(&mut HashMap::new(), &bags, &"shiny gold".to_string()) - 1
    );
}
