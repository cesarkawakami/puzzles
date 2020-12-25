use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug)]
struct Recipe {
    ingredients: HashSet<String>,
    allergens: HashSet<String>,
}

fn parse_recipe(line: &str) -> Recipe {
    assert!(line.ends_with(")"));
    let (ingredient_str, allergens_str) =
        if let [left, right] = line.splitn(2, "(contains").collect::<Vec<_>>().as_slice() {
            (left.trim(), right.trim_end_matches(&[' ', ')'][..]))
        } else {
            panic!();
        };

    let ingredients: HashSet<_> = ingredient_str
        .split(' ')
        .map(|s| s.trim().to_string())
        .collect();
    let allergens: HashSet<_> = allergens_str
        .split(",")
        .map(|s| s.trim().to_string())
        .collect();
    Recipe {
        ingredients,
        allergens,
    }
}

fn get_possible_pairings<'a>(recipes: &'a Vec<Recipe>) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut possibilities: HashMap<&str, HashSet<&str>> = HashMap::new();
    for recipe in recipes {
        for allergen in &recipe.allergens {
            let recipe_ingredients: HashSet<&str> =
                recipe.ingredients.iter().map(|s| s.as_str()).collect();
            let updated_possibilities: HashSet<&str> = match possibilities.get(allergen.as_str()) {
                None => recipe_ingredients,
                Some(current) => current.intersection(&recipe_ingredients).copied().collect(),
            };
            possibilities.insert(&allergen, updated_possibilities);
        }
    }
    possibilities
}

fn get_unsafe_ingredients<'a>(recipes: &'a Vec<Recipe>) -> HashSet<&'a str> {
    let possibilities = get_possible_pairings(recipes);
    possibilities
        .into_iter()
        .map(|(_, v)| v)
        .fold(HashSet::new(), |acc, set| {
            acc.union(&set).copied().collect()
        })
}

fn get_safe_ingredients<'a>(recipes: &'a Vec<Recipe>) -> HashSet<&'a str> {
    let all_ingredients: HashSet<&str> = recipes
        .iter()
        .flat_map(|r| r.ingredients.iter().map(|s| s.as_str()))
        .collect();
    let unsafe_ingredients = get_unsafe_ingredients(recipes);
    all_ingredients
        .difference(&unsafe_ingredients)
        .copied()
        .collect()
}

fn solve_part2<'a>(
    possible_pairings: &'a HashMap<&'a str, HashSet<&'a str>>,
) -> HashMap<&'a str, &'a str> {
    let mut pairings = possible_pairings.clone();
    let expected = pairings.len();
    let mut output: HashMap<&str, &str> = HashMap::new();
    while output.len() < expected {
        let (&ingredient, &allergen) = match pairings.iter().find(|(_, v)| v.len() == 1) {
            None => panic!(),
            Some((k, v)) => (k, v.iter().next().unwrap()),
        };
        pairings.remove(ingredient);
        output.insert(ingredient, allergen);
        for (_, allergens) in &mut pairings {
            allergens.remove(allergen);
        }
    }
    output
}

fn main() {
    let reader = BufReader::new(std::io::stdin());
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.unwrap().trim().to_string())
        .collect();
    let recipes: Vec<Recipe> = lines
        .iter()
        .map(|line| parse_recipe(line.as_str()))
        .collect();

    let safe_ingredients = get_safe_ingredients(&recipes);
    let appearance_count = safe_ingredients.iter().fold(0, |acc, ingredient| {
        acc + recipes
            .iter()
            .filter(|r| r.ingredients.contains(*ingredient))
            .count()
    });

    println!("part1, appearance count: {}", appearance_count);

    let possible_pairings = get_possible_pairings(&recipes);
    println!("part2, pairings:");
    println!("{:?}", possible_pairings);

    let final_pairings = solve_part2(&possible_pairings);
    let ordered_pairings: BTreeMap<&str, &str> = final_pairings
        .iter()
        .map(|(&k, &v)| (k, v))
        .collect();
    for (k, v) in &ordered_pairings {
        println!("{} contains {}", v, k);
    }

    let canonical_list = ordered_pairings
        .iter()
        .map(|(&_, &v)| v)
        .collect::<Vec<_>>()
        .join(",");
    println!("part2, list: {}", canonical_list);
}
