use std::collections::HashMap;
use std::cmp::{max, min};

#[derive(Debug, Default, Copy, Clone, Eq, Hash, PartialEq)]
struct Point(i32, i32);
struct Op(Point, Point);

fn get_input(input: &str) -> Vec<Op> {
    input
      .lines()
      .map(
        |line| {
          let parts: Vec<Vec<i32>> = line.split(" -> ")
            .map(
              |part| 
                part.split(",").map(|p| p.parse::<i32>().unwrap()).collect()
            ).collect();
          Op(Point(parts[0][0], parts[0][1]), Point(parts[1][0], parts[1][1]))
        }
      ).collect()
}

fn range_points(p0: Point, p1: Point, diag: bool) -> Option<Vec<Point>> {
  let x0 = min(p0.0, p1.0);
  let y0 = min(p0.1, p1.1);
  let x1 = max(p0.0, p1.0);
  let y1 = max(p0.1, p1.1);
  if x0 == x1 {
    return Some((y0..=y1).map(|y| Point(x0, y)).collect());
  } else if y0 == y1 {
    return Some((x0..=x1).map(|x| Point(x, y0)).collect());
  } else if diag && (x1 - x0) == (y1 - y0) {
    let x_step = if p0.0 > p1.0 { -1 } else { 1 };
    let y_step = if p0.1 > p1.1 { -1 } else { 1 };
    let mut x = p0.0;
    let mut y = p0.1;
    let mut points = Vec::new();
    for i in 0..=(x1 - x0) {
      points.push(Point(x, y));
      x += x_step;
      y += y_step;
    }
    return Some(points);
  } else {
    None
  }
}

fn insert_grid(grid: &mut HashMap<Point, i32>, point: &Point) {
    grid.entry(*point).and_modify(|e| *e += 1).or_insert(1);
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let mut gridset = HashMap::new();
  for op in input {
    if let Some(range) = range_points(op.0, op.1, false) {
      for i in range{
        insert_grid(&mut gridset, &Point(i.0, i.1));
      }
    }
  }
  let part1 = gridset.values().filter(|&x| *x >= 2).count();
  dbg!(part1);
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let mut gridset = HashMap::new();
  for op in input {
    if let Some(range) = range_points(op.0, op.1, true) {
      for i in range{
        insert_grid(&mut gridset, &Point(i.0, i.1));
      }
    }
  }
  let part2 = gridset.values().filter(|&x| *x >= 2).count();
  dbg!(part2);
}