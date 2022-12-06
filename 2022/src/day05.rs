use std::{collections::HashMap, cell::RefCell};

#[derive(Copy, Clone, Debug)]
struct Move {
  amount: usize,
  source: usize,
  dest: usize
}

fn parse_moves(input: &str) -> Vec<Move> {
  input.split("\n").filter(|s| !s.is_empty())
    .map(|line| {
      let chars = line.chars().collect::<Vec<_>>();
      Move {
        amount: line.split(" ").skip(1).next().unwrap().parse().unwrap(),
        source: chars[chars.len() - 6].to_digit(10).unwrap() as usize,
        dest: chars[chars.len() - 1].to_digit(10).unwrap() as usize,
      }
    }).collect()
}

fn parse_stack(input: &str) -> HashMap<usize, RefCell<Vec<char>>> {
  let mut stacks = HashMap::new();
  let mut lines = input
    .split("\n")
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>();

  lines.reverse();

  for i in 1..=9usize {
    stacks.insert(i, RefCell::new(Vec::new()));
  }

  for line in lines.iter().skip(1) {
    let chars = line.chars().collect::<Vec<_>>();
    for idx in (1..chars.len()).step_by(4) {
      if chars[idx].is_ascii_uppercase() {
        let pos = (idx - 1) / 4 + 1;
        stacks
          .get(&pos)
          .unwrap()
          .borrow_mut()
          .push(chars[idx]);
      }
    }
  }
  stacks
}

pub fn part1(input: String) { 
  let parts = input.split("\n\n").collect::<Vec<_>>();

  let stacks = parse_stack(parts[0]);
  let moves = parse_moves(parts[1]);
  
  for step in moves {
    let mut source = stacks.get(&step.source).unwrap().borrow_mut();
    let mut dest = stacks.get(&step.dest).unwrap().borrow_mut();

    for _ in 0..step.amount {
      let val = source.pop().unwrap();
      dest.push(val);
    }
  }
  let mut top = String::new();
  for i in 1..=9usize {
    let stack = stacks.get(&i).unwrap().borrow();
    if !stack.is_empty() {
      top.push(
        stack[stack.len() - 1]
      )
    }
  }
  dbg!(top);
}

pub fn part2(input: String) {
  let parts = input.split("\n\n").collect::<Vec<_>>();

  let stacks = parse_stack(parts[0]);
  let moves = parse_moves(parts[1]);
  
  for step in moves {
    let mut source = stacks.get(&step.source).unwrap().borrow_mut();
    let mut dest = stacks.get(&step.dest).unwrap().borrow_mut();

    let n = source.len();
    dest.extend_from_slice(&source[(n-step.amount)..n]);
    source.truncate(n-step.amount);
  }
  let mut top = String::new();
  for i in 1..=9usize {
    let stack = stacks.get(&i).unwrap().borrow();
    if !stack.is_empty() {
      top.push(
        stack[stack.len() - 1]
      )
    }
  }
  dbg!(top);
}
