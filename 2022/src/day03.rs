use std::{collections::HashSet, iter::FromIterator};

fn get_value(item: u32) -> u32 {
  if item > 96 {
    return item - 96;
  }
  item - 64 + 26
}

pub fn part1(input: String) { 
  let ans: u32 = input
    .split("\n")
    .filter(|s| !s.is_empty())
    .map(|sack| {
      let len = sack.len();
      let comp1: HashSet<char> = HashSet::from_iter(sack[0..len/2].chars());
      let common = sack[len/2..].chars().find(|item| comp1.contains(item)).unwrap();
      get_value(common as u32)
    }).sum();
  
  dbg!(ans);
}

pub fn part2(input: String) {
  let ans: u32 = input
    .split("\n")
    .filter(|s| !s.is_empty())
    .collect::<Vec<&str>>()
    .chunks(3)
    .map(|group| {
      let sack1: HashSet<char> = HashSet::from_iter(group[0].chars());
      let sack2: HashSet<char> = HashSet::from_iter(group[1].chars());
      let common = group[2].chars().find(|item| sack1.contains(item) && sack2.contains(item)).unwrap();
      get_value(common as u32)
    })
    .sum();

    dbg!(ans);
}
