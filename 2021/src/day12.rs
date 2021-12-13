use std::collections::{BTreeSet, HashMap};

fn get_input(input: &str) -> HashMap<&str, Vec<&str>> {
  let mut map = HashMap::new();
  for line in input.lines() {
    let (start, end) = line.split_once('-').unwrap();
    map.entry(start).or_insert(Vec::new()).push(end);
    map.entry(end).or_insert(Vec::new()).push(start);
  }
  map
}

fn is_lowercase(name: &str) -> bool {
  return name.as_bytes()[0] >= b'a'
}

fn traverse<'a>(curr: &'a str, input: &HashMap<&'a str, Vec<&'a str>>, visited: &mut BTreeSet<&'a str>, double_flag: bool) -> usize {
  let seen = visited.contains(&curr);
  match curr {
    "end" => 1,
    _ if seen && !double_flag => 0,
    _ => {
      let inserted = !seen && is_lowercase(curr) && visited.insert(curr);
      let count = input[&curr].iter()
        .filter(|&&next| next != "start")
        .map(|&next| traverse(next, input, visited, double_flag && !seen))
        .sum();
      if inserted {
        visited.remove(&curr);
      }
      count
    }
  }
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let part1 = traverse("start", &input, &mut BTreeSet::new(), false);
  dbg!(part1);
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let part2 = traverse("start", &input, &mut BTreeSet::new(), true);
  dbg!(part2);
}