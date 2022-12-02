pub fn part1(input: String) { 
  let inventories = input.split("\n\n");
  let max = inventories.map(|inventory| {
    inventory
      .split("\n")
      .filter(|&s| !s.is_empty())
      .map(|s| s.parse::<u64>().unwrap())
      .sum::<u64>()
  }).max().unwrap();
  dbg!(max);
}

pub fn part2(input: String) {
  let inventories = input.split("\n\n");
  let mut rations = inventories.map(|inventory| {
    inventory
      .split("\n")
      .filter(|&s| !s.is_empty())
      .map(|s| s.parse::<u64>().unwrap())
      .sum::<u64>()
  }).collect::<Vec<u64>>();
  rations.sort();
  let max: u64 = rations.iter().rev().take(3).sum();
  dbg!(max);
}
