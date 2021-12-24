use std::collections::VecDeque;

pub fn part1(input: String) {
  let instructions = input.lines().collect::<Vec<_>>();
  let mut part1 = 99999999999999i128;
  let mut part2 = 11111111111111i128;
  let mut stack = VecDeque::new();

  for i in 0..14 {
    let a = instructions[18 * i + 5].split_whitespace().last().unwrap().parse::<i32>().unwrap();
    let b = instructions[18 * i + 15].split_whitespace().last().unwrap().parse::<i32>().unwrap();
    if a > 0 {
      stack.push_back((i, b));
      continue;
    }
    let (j, b) = stack.pop_back().unwrap();
    let sum = (a + b) as i128;
    part1 -= (
      sum * 10i128.pow(13 - (if a > -b { j } else { i }) as u32)
    ).abs();
    part2 += (
      sum * 10i128.pow(13 - (if a < -b { j } else { i }) as u32)
    ).abs();
  }

  dbg!(part1);
  dbg!(part2);
}

pub fn part2(input: String) {
}