use std::collections::HashSet;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Default)]
struct Point(i32, i32, i32);

enum Directions { X, Y, Z }

impl Point {
  fn neighbors(&self) -> HashSet<Point> {
    [Directions::X, Directions::Y, Directions::Z]
      .iter()
      .flat_map(|dir| {
        [-1, 1]
          .iter()
          .map(move |x| {
            let mut res = self.clone();
            match dir {
              Directions::X => res.0 += *x,
              Directions::Y => res.1 += *x,
              Directions::Z => res.2 += *x,
            }
            res.into()
          })
      })
      .collect()
  }
}

fn parse_input(input: &str) -> HashSet<Point> {
  input
    .lines()
    .map(|line| {
      let mut parts = line.split(",").map(|s| s.parse().unwrap());
      Point(
        parts.next().unwrap(),
        parts.next().unwrap(),
        parts.next().unwrap()
      )
    })
    .collect()
}

pub fn part1(input: String) { 
  let pts = parse_input(&input);
  let ans = pts
    .iter()
    .flat_map(|pt| pt.neighbors())
    .filter(|pt| !pts.contains(pt))
    .count();
  dbg!(ans);
}

fn bounds(cubes: &HashSet<Point>) -> (Point, Point) {
  cubes
    .iter()
    .fold(
      (Point::default(), Point::default()),
      |(mut min, mut max), val| {
        min.0 = min.0.min(val.0);
        min.1 = min.1.min(val.1);
        min.2 = min.2.min(val.2);
        max.0 = max.0.max(val.0);
        max.1 = max.1.max(val.1);
        max.2 = max.2.max(val.2);
        (min, max)
      }
    )
}

impl Point {
  fn in_bounds(&self, (mins, maxs): &(Point, Point)) -> bool {
    self.0 <= maxs.0 + 1 &&
    self.1 <= maxs.1 + 1 &&
    self.2 <= maxs.2 + 1 &&
    self.0 >= mins.0 - 1 &&
    self.1 >= mins.1 - 1 &&
    self.2 >= mins.2 - 1
  }
}

fn exposed(pts: &HashSet<Point>) -> HashSet<Point> {
  let mut exposed = HashSet::new();
  let bounds = bounds(&pts);
  let start = Point::default();
  let mut stack = vec!();
  stack.push(start);
  exposed.insert(start);

  while let Some(next) = stack.pop() {
    for neighbor in next.neighbors() {
      if pts.contains(&neighbor) || !neighbor.in_bounds(&bounds) {
        continue;
      }
      if exposed.insert(neighbor) {
        stack.push(neighbor);
      }
    }
  }

  exposed
}

pub fn part2(input: String) {
  let pts = parse_input(&input);
  let exposed = exposed(&pts);
  let ans = pts
    .iter()
    .flat_map(|pt| pt.neighbors())
    .filter(|pt| exposed.contains(pt))
    .count();
  dbg!(ans);
}
