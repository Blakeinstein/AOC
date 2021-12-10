use std::collections::{LinkedList, HashSet, HashMap};

pub fn part1(input: String) {
  let opening = ['{', '<', '[', '('].iter().cloned().collect::<HashSet<char>>();
  let pairs = HashMap::from([('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]);
  let scores = HashMap::from([(')', 3), (']', 57), ('}', 1197), ('>', 25137)]);
  let mut part1 = 0;
  for line in input.lines() {
    let mut stack = LinkedList::new();
    for c in line.chars() {
      if opening.contains(&c) {
        stack.push_back(c);
      } else {
        let opening = stack.back().unwrap_or(&'#');
        if c == *pairs.get(opening).unwrap_or(&'#') {
          stack.pop_back();
        } else {
          part1 += scores.get(&c).unwrap();
          break;
        }
      }
    }
  }
  dbg!(part1);
}

pub fn part2(input: String) {
  let opening = ['{', '<', '[', '('].iter().cloned().collect::<HashSet<char>>();
  let pairs = HashMap::from([('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]);
  let scores = HashMap::from([('(', 1), ('[', 2), ('{', 3), ('<', 4)]);
  let mut scores_list = vec!();
  for line in input.lines() {
    let mut stack = LinkedList::new();
    for c in line.chars() {
      if opening.contains(&c) {
        stack.push_back(c);
      } else {
        let opening = stack.back().unwrap_or(&'#');
        if c == *pairs.get(opening).unwrap_or(&'#') {
          stack.pop_back();
        } else {
          stack.clear();
          break;
        }
      }
    }
    let mut curr_score: u64 = 0;
    while let Some(c) = stack.pop_back() {
      curr_score = curr_score * 5 + scores.get(&c).unwrap();
    }
    if curr_score > 0 {
      scores_list.push(curr_score);
    }
  }
  scores_list.sort_unstable();
  let part2 = scores_list[scores_list.len() / 2];
  dbg!(part2);
}