use std::collections::HashMap;

#[derive(Debug)]
struct Puzzle {
  starter: String,
  rules: HashMap<String, String>,
}

fn get_input(input: &str) -> Puzzle {
  let mut lines = input.lines();
  let starter = lines.next().unwrap().to_string();
  let rules = lines.skip(1).fold(HashMap::new(), |mut map, line| {
    let (a, b) = line.split_once(" -> ").unwrap();
    map.insert(a.to_string(), b.to_string());
    map
  }).to_owned();
  Puzzle{
    starter,
    rules
  }
}

fn fold_polymer(puzzle: &Puzzle, steps: usize) -> i64 {
  let mut counts = puzzle.starter.chars().fold(HashMap::new(), |mut map, c| {
    *map.entry(c.to_string()).or_insert(0) += 1i64;
    map
  });
  let mut pairs = puzzle.starter.chars()
    .zip(puzzle.starter.chars().skip(1))
    .fold(HashMap::new(), |mut map, (a, b)| {
      *map.entry(format!("{}{}", a, b)).or_insert(0) += 1;
      map
    });
  for i in 0..steps {
    let pairs_copy = pairs.clone();
    for (key, count) in pairs_copy.iter() {
      if *count > 0 {
        *pairs.get_mut(key).unwrap() -= count;
        let next = &puzzle.rules[key];
        *counts.entry(next.to_string()).or_insert(0) += count;
        let parts = key.chars().take(2).collect::<Vec<_>>();
        *pairs.entry(format!("{}{}", parts[0], next)).or_insert(0) += count;
        *pairs.entry(format!("{}{}", next, parts[1])).or_insert(0) += count;
      }
    };
  }
  counts.values().max().unwrap() - counts.values().min().unwrap()
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let part1 = fold_polymer(&input, 10);
  dbg!(part1);
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let part1 = fold_polymer(&input, 40);
  dbg!(part1);
}