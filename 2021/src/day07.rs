fn get_input(input: &str) -> Vec<i32> {
  input
    .lines().next().unwrap()
    .split(',')
    .map(|x| x.parse::<i32>().unwrap())
    .collect()
}

pub fn part1(input: String) {
  let mut input = get_input(&input);
  input.sort_unstable();
  let opt_pos = input[input.len() / 2];
  let part1 = input.iter().map(|x| (x - opt_pos).abs()).sum::<i32>();
  dbg!(part1);
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let opt_pos = input.iter().sum::<i32>() / input.len() as i32;
  let part2 = input.iter().map(|x| {
    let n = (x - opt_pos).abs();
    n * (n + 1) / 2
  }).sum::<i32>();
  dbg!(part2);
}