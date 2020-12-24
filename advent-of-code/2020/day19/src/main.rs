use std::collections::BTreeMap;
use std::fmt::Debug;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug)]
struct RuleSet {
    rules: BTreeMap<i32, RuleGrammar>,
}

#[derive(Debug)]
enum RuleGrammar {
    Literal(char),
    Call(i32),
    Concat(Box<RuleGrammar>, Box<RuleGrammar>),
    Altern(Box<RuleGrammar>, Box<RuleGrammar>),
}

#[derive(Debug)]
enum SExpr<'a, T: 'a> {
    Nil,
    Cons(T, &'a SExpr<'a, T>),
}

impl RuleSet {
    fn parse_rule_line(&mut self, line: &str) {
        let (id, grammar_str): (i32, _) = match line.splitn(2, ":").collect::<Vec<_>>().as_slice() {
            [id_str, grammar_str] => (id_str.trim().parse().unwrap(), grammar_str.trim()),
            _ => panic!(),
        };

        let tokens: Vec<_> = grammar_str.split(' ').collect();
        let grammar = self.parse_rule_tokens(&tokens);
        self.rules.insert(id, grammar);
    }

    fn parse_rule_tokens(&mut self, tokens: &[&str]) -> RuleGrammar {
        let mut stack: Vec<RuleGrammar> = vec![];
        let mut insert_new = true;

        fn do_add(stack: &mut Vec<RuleGrammar>, insert_new: &mut bool, to_add: RuleGrammar) {
            if *insert_new {
                stack.push(to_add);
                *insert_new = false;
            } else {
                let left = stack.pop().unwrap();
                stack.push(RuleGrammar::Concat(Box::new(left), Box::new(to_add)));
            }
        }

        tokens.iter().for_each(|token| {
            if *token == "|" {
                insert_new = true;
            } else if token.starts_with("\"") {
                let ch = token.chars().nth(1).unwrap();
                do_add(&mut stack, &mut insert_new, RuleGrammar::Literal(ch));
            } else if let Ok(id) = token.parse::<i32>() {
                do_add(&mut stack, &mut insert_new, RuleGrammar::Call(id));
            } else {
                panic!("unable to parse token: {}", token);
            }
        });

        assert!(!insert_new);

        stack
            .into_iter()
            .fold(None, |acc, x| match acc {
                None => Some(x),
                Some(acc) => Some(RuleGrammar::Altern(Box::new(acc), Box::new(x))),
            })
            .unwrap()
    }

    fn check_rule(&self, id: i32, s: &str) -> bool {
        self.check_rule_stack(
            &SExpr::Cons(&RuleGrammar::Call(id), &SExpr::Nil),
            s.chars().collect::<Vec<_>>().as_slice(),
        )
    }

    fn check_rule_stack<'a, 'b: 'a>(&'b self, stack: &SExpr<&'a RuleGrammar>, s: &[char]) -> bool {
        match stack {
            SExpr::Nil => s.is_empty(),
            SExpr::Cons(RuleGrammar::Literal(ch), rest) => {
                if !s.is_empty() && s[0] == *ch {
                    self.check_rule_stack(rest, &s[1..])
                } else {
                    false
                }
            }
            SExpr::Cons(RuleGrammar::Call(id), rest) => {
                self.check_rule_stack(&SExpr::Cons(&self.rules.get(&id).unwrap(), rest), s)
            }
            SExpr::Cons(RuleGrammar::Concat(left, right), rest) => {
                self.check_rule_stack(&SExpr::Cons(&left, &SExpr::Cons(&right, rest)), s)
            }
            SExpr::Cons(RuleGrammar::Altern(left, right), rest) => {
                if self.check_rule_stack(&SExpr::Cons(&left, rest), s) {
                    true
                } else {
                    self.check_rule_stack(&SExpr::Cons(&right, rest), s)
                }
            }
        }
    }
}

fn main() {
    let reader = BufReader::new(std::io::stdin());
    let lines: Vec<String> = reader
        .lines()
        .map(|maybe_line| maybe_line.unwrap().trim().to_string())
        .collect();
    let rule_lines: Vec<&str> = lines
        .iter()
        .take_while(|line| !line.is_empty())
        .map(|line| line.as_str())
        .collect();
    let test_string_lines: Vec<&str> = lines
        .iter()
        .skip_while(|line| !line.is_empty())
        .skip(1)
        .map(|line| line.as_str())
        .collect();

    let mut rule_set = RuleSet {
        rules: BTreeMap::new(),
    };
    for line in rule_lines {
        rule_set.parse_rule_line(line);
    }

    let match_results: Vec<bool> = test_string_lines
        .iter()
        .map(|line| rule_set.check_rule(0, line))
        .collect();

    for (test_string, match_result) in test_string_lines.iter().zip(match_results.iter()) {
        println!("part1, {}: {}", test_string, match_result);
    }
    println!("part1, true count: {}", match_results.iter().filter(|x| **x).count());
    println!();

    // wtf?
    rule_set.parse_rule_line("8: 42 | 42 8");
    rule_set.parse_rule_line("11: 42 31 | 42 11 31");
    let match_results_part2: Vec<bool> = test_string_lines
        .iter()
        .map(|line| rule_set.check_rule(0, line))
        .collect();

    for (test_string, match_result) in test_string_lines.iter().zip(match_results_part2.iter()) {
        println!("part2, {}: {}", test_string, match_result);
    }
    println!("part2, true count: {}", match_results_part2.iter().filter(|x| **x).count());
}
