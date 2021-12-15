use std::collections::BinaryHeap;

fn get_input(input: &str) -> Vec<Vec<u32>>{
  input
    .lines()
    .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect::<Vec<_>>())
    .collect::<Vec<_>>()
}

fn solve_part1(input: &str) -> i32 {
  let map = get_input(input);
  let goal = (map.len() -1, map[0].len() - 1);
  let mut dist = vec![vec![i32::MAX; map[0].len()]; map.len()];
  let mut q = BinaryHeap::new();
  q.push((0,0,0));
  while let Some((cost,x,y)) = q.pop() {
    if (x,y) == goal { 
      return - cost; 
    }
    if - cost > dist[x][y] { 
      continue; 
    }
    for (x1, y1) in [
        (x.wrapping_sub(1), y),
        (x, y.wrapping_sub(1)),
        (x+1, y),
        (x, y+1)
      ] {
      if map.get(x1).and_then(|row| row.get(y1)).is_none() {
        continue;
      }
      let next_cost = - cost + map[x1][y1] as i32;
      if next_cost < dist[x1][y1] {
        q.push((-next_cost, x1, y1));
        dist[x1][y1] = next_cost;
      }
    }
  }
  unreachable!();
}

pub fn part1(input: String) {
  let part1 = solve_part1(&input);
  dbg!(part1);
}

fn get_input2(input: &str) -> String {
  let mut s = String::new();
  for i in 0..5 {
    for line in input.lines() {
      for j in 0..5 {
        for risk in line.chars() {
          let risk = risk.to_digit(10).unwrap() + i + j;
          let risk = if risk > 9 { risk - 9 } else { risk };
          s.push(char::from_digit(risk, 10).unwrap());
        }
      }
      s.push('\n');
    }
  }
  return s;
}


pub fn part2(input: String) {
  let new_input = get_input2(&input);
  let part2 = solve_part1(&new_input);
  dbg!(part2);
}