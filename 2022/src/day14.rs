use std::collections::{HashSet};

type Point = (usize, usize);

fn make_point(point: &str) -> Point {
  let parts = point
    .split(",")
    .collect::<Vec<_>>();
  
  (parts[0].parse().unwrap(), parts[1].parse().unwrap())
}

fn parse_input(input: &str) -> (HashSet<Point>, usize) {
  let seqs: Vec<Vec<Point>> = input
    .split("\n")
    .filter(|s| !s.is_empty())
    .map(|seq| seq
      .split(" -> ")
      .map(make_point)
      .collect()
    )
    .collect();


  let mut rocks: HashSet<Point> = HashSet::new();
  let mut lim = 0;

  for seq in seqs.iter() {
    for window in seq.windows(2) {
      let (ax, ay) = window[0];
      let (bx, by) = window[1];
      assert!(ax == bx || ay == by);

      for x in ax.min(bx)..=ax.max(bx) {
        rocks.insert((x, ay));
        rocks.insert((x, by));
      }

      for y in ay.min(by)..=ay.max(by) {
        rocks.insert((ax, y));
        rocks.insert((bx, y));
      }
      lim = lim.max(ay).max(by);
    }
  }
  (rocks, lim)
}

const DIRECTIONS: &'static [(isize, isize)] = &[(0, 1), (-1, 1), (1, 1)];

fn simulate_sand(rocks: &HashSet<Point>, limit: &usize, is_floor: bool) -> Option<(usize, usize)> {
  let mut curr: Point = (500, 0);

  loop {
    if !is_floor && curr.1 >= *limit {
      return None;
    }
    let mut flag = false;
    for (dx, dy) in DIRECTIONS.iter() {
      let next = (curr.0.checked_add_signed(*dx).unwrap(), curr.1.checked_add_signed(*dy).unwrap());
      if !rocks.contains(&next) {
        curr = next;
        flag = true;
        break;
      }
    }
    if !flag || (is_floor && curr.1 == *limit - 1) {
      break;
    }
  }

  if rocks.contains(&curr) {
      None
  } else {
      Some(curr)
  }
}

pub fn part1(input: String) { 
  let (mut rocks, lim) = parse_input(&input);
  
  let curr = rocks.len();
  while let Some((nx, ny)) = simulate_sand(&rocks, &lim, false) {
    rocks.insert((nx, ny));
  }
  let ans = rocks.len() - curr;
  dbg!(ans);
}

pub fn part2(input: String) {
  let (mut rocks, lim) = parse_input(&input);
  let new_lim = lim + 2;
  let curr = rocks.len();
  while let Some((nx, ny)) = simulate_sand(&rocks, &new_lim, true) {
    rocks.insert((nx, ny));
  }
  let ans = rocks.len() - curr;
  dbg!(ans);
}
