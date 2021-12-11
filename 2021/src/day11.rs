use std::collections::HashSet;
use std::convert::TryInto;

fn get_input(input: &str) -> [[u32; 10]; 10] {
  input
    .lines()
    .map(|line| {
      line
        .chars()
        .map(|x| x.to_digit(10).unwrap())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
    })
    .collect::<Vec<_>>()
    .try_into()
    .unwrap()
}

fn flash(map: &mut [[u32; 10]; 10], i: usize, j: usize, blink_set: &mut HashSet<(usize, usize)>) {
  if i > 9 ||
    j > 9 ||
    blink_set.contains(&(i, j)) {
    return
  }
  map[i][j] += 1;
  if map[i][j] > 9 {
    blink_set.insert((i, j));
    map[i][j] = 0;
    flash(map, i + 1, j, blink_set); // ->
    flash(map, i + 1, j + 1, blink_set); // ->^
    flash(map, i, j + 1, blink_set); // ^
    flash(map, i.wrapping_sub(1), j + 1, blink_set); // <-^
    flash(map, i.wrapping_sub(1), j, blink_set); // <-
    flash(map, i.wrapping_sub(1), j.wrapping_sub(1), blink_set); // <-v
    flash(map, i, j.wrapping_sub(1), blink_set); // v
    flash(map, i + 1, j.wrapping_sub(1), blink_set); // ->v
  }
}

pub fn part1(input: String) {
  let mut input = get_input(&input);
  let mut part1 = 0;
  let mut blink_set = HashSet::new();
  for _ in 0..100 {
    for i in 0..10 {
      for j in 0..10 {
        flash(&mut input, i, j, &mut blink_set);
      }
    }
    part1 += blink_set.len();
    blink_set.clear();
  }
  dbg!(part1);
}

pub fn part2(input: String) {
  let mut input = get_input(&input);
  let mut part2= 0;
  let mut blink_set = HashSet::new();
  while blink_set.len() < 10 * 10 {
    blink_set.clear();
    for i in 0..10 {
      for j in 0..10 {
        flash(&mut input, i, j, &mut blink_set);
      }
    }
    part2 += 1;
  }
  dbg!(part2);
}