use std::collections::{HashMap, HashSet};

struct Problem {
  input: Vec<Vec<char>>,
  output: Vec<Vec<char>>,
}

// (0, 6),
// (1, 2),
// (2, 5),
// (3, 5),
// (4, 4),
// (5, 5),
// (6, 6),
// (7, 3),
// (8, 7),
// (9, 6)

fn get_input(input: &str) -> Vec<Problem> {
  input
    .lines()
    .map(
      |line| {
        let mut parts = line.split(" | ");
        let input = parts.next().unwrap().split_whitespace().map(|x| x.chars().collect()).collect();
        let output = parts.next().unwrap().split_whitespace().map(|x| x.chars().collect()).collect();
        Problem {
          input, output
        }
      }
    )
    .collect()
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let mut part1 = 0;
  let mut unique = HashSet::new();
  unique.insert(2);
  unique.insert(3);
  unique.insert(4);
  unique.insert(7);
  for problem in input.iter() {
    for code in problem.output.iter() {
      if unique.contains(&code.len()) {
        part1 += 1;
      }
    }
  }
  dbg!(part1);
}

fn to_set(x: &Vec<char>) -> HashSet<char> {
  x.into_iter().cloned().collect::<HashSet<char>>()
}

fn parse_truth(input: &Vec<Vec<char>>, output: &Vec<Vec<char>>) -> u32 {
  let mut chars = vec!['0'; 4];

  let mut map = HashMap::new();
  let mut by_size: HashMap<usize, Vec<Vec<char>>> = HashMap::new();
  for code in input.iter() {
    by_size.entry(code.len()).or_insert(vec![]).push(code.to_vec());
  }

  let one = to_set(by_size.get(&2).unwrap().iter().next().unwrap());
  let seven = to_set(by_size.get(&3).unwrap().iter().next().unwrap());
  let four = to_set(by_size.get(&4).unwrap().iter().next().unwrap());

  for code in by_size.get(&5).unwrap().iter() {
    let curr_set = to_set(code);
    if one.intersection(&curr_set).count() == 2 {
      map.insert('3', curr_set.clone());
    } else if four.intersection(&curr_set).count() == 3 {
      map.insert('5', curr_set.clone());
    } else {
      map.insert('2', curr_set.clone());
    }
  }

  for code in by_size.get(&6).unwrap().iter() {
    let curr_set = to_set(code);
    if four.intersection(&curr_set).count() == 4 {
      map.insert('9', curr_set.clone());
    } else if seven.intersection(&curr_set).count() == 2 {
      map.insert('6', curr_set.clone());
    } else {
      map.insert('0', curr_set.clone());
    }
  }

  for (i, codes) in output.iter().enumerate() {
    chars[i] = match codes.len() {
      7 => '8',
      3 => '7',
      4 => '4',
      2 => '1',
      6 => {
        let curr_set = to_set(codes);
        *['0', '9', '6'].iter().find(|&x| map.get(x).unwrap().intersection(&curr_set).count() == 6).unwrap()
      },
      5 => {
        let curr_set = to_set(codes);
        *['2', '3', '5'].iter().find(|&x| map.get(x).unwrap().intersection(&curr_set).count() == 5).unwrap()
      }
      _ => panic!("Unknown length"),
    }
  }
  u32::from_str_radix(&chars.iter().collect::<String>(), 10).unwrap()
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let mut part2 = 0;
  for problem in input.iter() {
    let truth = parse_truth(&problem.input, &problem.output);
    part2 += truth;
  }
  dbg!(part2);
}