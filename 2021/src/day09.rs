use std::collections::BinaryHeap;
use std::collections::VecDeque;

fn get_input(input: &str) -> Vec<Vec<u32>> {
  input
    .lines()
    .map(|line|
      line.chars().map(|x| x.to_digit(10).unwrap()).collect::<Vec<u32>>()
    ).collect()
}

fn risk_level(input: &Vec<Vec<u32>>, i: usize, j: usize) -> bool {
  let curr = input[i][j];
  if 
    (i == 0 || input[i - 1][j] > curr) &&
    (i == input.len() - 1 || input[i + 1][j] > curr) &&
    (j == 0 || input[i][j - 1] > curr) &&
    (j == input[i].len() - 1 || input[i][j + 1] > curr) {
      true
    }
    else {
      false
    }
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let m = input.len();
  let n = input[0].len();
  let mut part1 = 0;
  for i in 0..m {
    for j in 0..n {
      part1 += if risk_level(&input, i, j) { input[i][j] + 1 } else { 0 };
    }
  }
  dbg!(part1);
}

fn get_basin_size(input: &mut Vec<Vec<u32>>, a: usize, b: usize) -> u32 {
  let mut basin_size = 0;
  let mut queue = VecDeque::new();
  queue.push_back((a, b));
  while let Some((i, j)) = queue.pop_front() {
    if input[i][j] == 9 { 
      continue; 
    }
    input[i][j] = 9;
    basin_size += 1;
    if i > 0 {
      queue.push_back((i - 1, j));
    }
    if i < input.len() - 1 {
      queue.push_back((i + 1, j));
    }
    if j > 0 {
      queue.push_back((i, j - 1));
    }
    if j < input[i].len() - 1 {
      queue.push_back((i, j + 1));
    }
  }
  basin_size
}

pub fn part2(input: String) {
  let mut input = get_input(&input);
  let m = input.len();
  let n = input[0].len();
  let mut heights = BinaryHeap::new();
  for i in 0..m {
    for j in 0..n {
      if risk_level(&input, i, j) {
        heights.push(get_basin_size(&mut input, i, j));
      }
    }
  }
  let mut part2 = 1;
  for _ in 0..3 {
    part2 *= heights.pop().unwrap();
  }
  dbg!(part2);
}