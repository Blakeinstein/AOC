
fn gen_input(input: String) -> Vec<i32> {
  input
    .lines()
    .map(|line| line.parse::<i32>().unwrap())
    .collect()
}
pub fn part1(input: String) {
  let input = gen_input(input);
  let mut now = input[0];
  let mut count = 0;
  for i in input {
    if i > now {
      count += 1;
    }
    now = i;
  }
  println!("{}", count);
}

pub fn part2(input: String) {
  let input = gen_input(input);
  let length = input.len();
  let mut now = input[0] + input[1] + input[2];
  let mut count = 0;
  for i in 0..length-2 {
    let next_sum = input[i] + input[i + 1] + input[i + 2];
    if next_sum > now {
      count += 1;
    }
    now = next_sum;
  }
  println!("{}", count);
}