fn get_input(input: &str) -> Vec<i32> {
  input
    .lines().next().unwrap()
    .split(',')
    .map(|x| x.parse::<i32>().unwrap())
    .collect()
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let mut part1 = i32::MAX;
  for pos in input.iter() {
    let curr_sum = input.iter().map(|x| (x - pos).abs()).sum::<i32>();
    part1 = part1.min(curr_sum);
  }
  dbg!(part1);
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let mut part2 = i32::MAX;
  let max_pos = input.iter().max().unwrap();
  for pos in 0..=*max_pos {
    let curr_sum = input.iter().map(|x| {
      let n = (x - pos).abs();
      n * (n + 1) / 2
    }).sum::<i32>();
    part2 = part2.min(curr_sum);
  }
  dbg!(part2);
}