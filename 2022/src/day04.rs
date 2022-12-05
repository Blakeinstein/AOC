pub fn part1(input: String) {
  let ans = input
  .split("\n")
  .filter(|s| !s.is_empty())
  .filter(|pair| {
    let values: Vec<u32> = pair.split(&['-', ','][..]).map(
      |s| s.parse::<u32>().unwrap()
    ).collect();
    (values[0] <= values[2] && values[3] <= values[1]) ||
    (values[0] >= values[2] && values[3] >= values[1])
  })
  .count();
  dbg!(ans);
}

pub fn part2(input: String) {
  let ans = input
  .split("\n")
  .filter(|s| !s.is_empty())
  .filter(|pair| {
    let values: Vec<u32> = pair.split(&['-', ','][..]).map(
      |s| s.parse::<u32>().unwrap()
    ).collect();
    values[0] <= values[3] &&
    values[2] <= values[1]
  })
  .count();
  dbg!(ans);
}