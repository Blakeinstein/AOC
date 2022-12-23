use std::collections::HashMap;

use either::Either;

type Point = (i32, i32);

type Map = HashMap<Point, bool>;

type Instruction = Either<i32, bool>;

fn min_x_for_row(y: i32, map: &Map) -> i32 {
  map
    .keys()
    .filter(|c| y == c.0)
    .map(|c| c.1)
    .min()
    .unwrap()
}

fn max_x_for_row(y: i32, map: &Map) -> i32 {
  map
    .keys()
    .filter(|c| y == c.0)
    .map(|c| c.1)
    .max()
    .unwrap()
}

fn min_y_for_col(x: i32, map: &Map) -> i32 {
  map
    .keys()
    .filter(|c| x == c.1)
    .map(|c| c.0)
    .min()
    .unwrap()
}

fn max_y_for_col(x: i32, map: &Map) -> i32 {
  map
    .keys()
    .filter(|c| x == c.1)
    .map(|c| c.0)
    .max()
    .unwrap()
}

fn add_points(p1: &Point, p2: &Point) -> Point {
  (p1.0 + p2.0, p1.1 + p2.1)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Direction {
  Top,
  Left,
  Right,
  Down
}

impl Direction {
  fn walk(&self, curr: &mut Point, map: &Map, mag: i32) {
    let diff = self.get_diff();
    for _ in 0..mag {
      let next = add_points(&curr, &diff);
      if let Some(is_wall) = map.get(&next) {
        if *is_wall {
          return;
        }
        *curr = next;
      } else {
        let next_square = match self {
          Direction::Top => (max_y_for_col(curr.1, map), curr.1),
          Direction::Left => (curr.0, max_x_for_row(curr.0, map)),
          Direction::Right => (curr.0, min_x_for_row(curr.0, map)),
          Direction::Down => (min_y_for_col(curr.1, map), curr.1),
        };
        if let Some(is_wall) = map.get(&next_square) {
          if *is_wall {
            return;
          }
          *curr = next_square;
        }
      }
    }
  }

  fn walk_cube(&self, curr: &mut Point, map: &Map, mag: i32) -> Self {
    let mut dir = *self;
    for _ in 0..mag {
      let diff = dir.get_diff();
      let next = add_points(&curr, &diff);
      if let Some(is_wall) = map.get(&next) {
        if *is_wall {
          break;
        }
        *curr = next;
      } else {
        let (next_sq, next_dir) = {
          if next.0 == -1 && next.1 > 49 && next.1 < 100 && dir == Direction::Top {
            ((next.1 + 100, 0), Direction::Right)
          } else if next.1 == 49 && next.0 < 50 && dir == Direction::Left {
            ((149 - next.0, 0), Direction::Right)
          } else if next.1 > 99 && next.0 == 50 && dir == Direction::Down {
            ((next.1 - 50, 99), Direction::Left)
          } else if next.1 > 99 && next.0 == -1 && dir == Direction::Top {
            ((199, next.1 - 100), Direction::Top)
          } else if next.1 > 149 && dir == Direction::Right {
            ((149 - next.0, 99), Direction::Left)
          } else if next.1 == 49 && next.0 > 49 && next.0 < 100 && dir == Direction::Left {
            ((100, next.0 - 50), Direction::Down)
          } else if next.1 == 100 && next.0 > 49 && next.0 < 100 && dir == Direction::Right {
            ((49, next.0 + 50), Direction::Top)
          } else if next.1 == 100 && next.0 > 99 && next.0 < 150 && dir == Direction::Right {
            ((149 - next.0, 149), Direction::Left)
          } else if next.0 == 150 && next.1 > 49 && next.1 < 100 && dir == Direction::Down {
            ((100 + next.1, 49), Direction::Left)
          } else if next.1 < 50 && next.0 < 100 && dir == Direction::Top {
            ((next.1 + 50, 50), Direction::Right)
          } else if next.1 < 0 && next.0 > 99 && next.0 < 150 && dir == Direction::Left {
            ((149 - next.0, 50), Direction::Right)
          } else if next.1 < 0 && next.0 > 149 && dir == Direction::Left {
            ((0, next.0 - 100), Direction::Down)
          } else if next.1 == 50 && next.0 > 149 && dir == Direction::Right {
            ((149, next.0 - 100), Direction::Top)
          } else if next.0 > 199 && dir == Direction::Down {
            ((0, next.1 + 100), Direction::Down)
          } else {
            panic!()
          }
        };
        if let Some(is_wall) = map.get(&next_sq) {
          if *is_wall {
            break;
          }
          *curr = next_sq;
          dir = next_dir;
        }
      }
    }
    dir
  }

  fn get_diff(&self) -> (i32, i32) {
    match self {
        Direction::Top => (-1, 0),
        Direction::Left => (0, -1),
        Direction::Right => (0, 1),
        Direction::Down => (1, 0),
    }
  }

  fn turn(self, right: bool) -> Self{
    if right {
      return match self {
        Direction::Top => Direction::Right,
        Direction::Left => Direction::Top,
        Direction::Right => Direction::Down,
        Direction::Down => Direction::Left,
      };
    }
    match self {
      Direction::Top => Direction::Left,
      Direction::Left => Direction::Down,
      Direction::Right => Direction::Top,
      Direction::Down => Direction::Right,
    }
  }
}


fn parse_input(input: &str) -> (Map, Vec<Instruction>) {
  let (map_str, dir_str) = input.split_once("\n\n").unwrap();

  let mut map = HashMap::new();

  for (i, line) in map_str.lines().enumerate() {
    for (j, c) in line.chars().enumerate() {
      if c == ' ' {
        continue;
      }
      map.insert((i as i32, j as i32), c == '#');
    }
  }

  let mut instructions = vec!();
  let mut curr = String::new();

  for c in dir_str.trim().chars() {
    if c.is_digit(10) {
      curr.push(c);
      continue;
    }
    if !curr.is_empty() {
      instructions.push(Instruction::Left(curr.parse().unwrap()));
      curr = String::new();
    }
    instructions.push(Instruction::Right(c == 'R'));
  }
  if !curr.is_empty() {
    instructions.push(Instruction::Left(curr.parse().unwrap()));
  }
  (map, instructions)
}

fn calc_score(curr: &Point, curr_dir: &Direction) -> i32 {
  let act = add_points(&curr, &(1, 1));

  act.0 * 1000 + act.1 * 4 + match curr_dir {
    Direction::Top => 3,
    Direction::Left => 2,
    Direction::Right => 0,
    Direction::Down => 1,
  }
}

pub fn part1(input: String) { 
  let (map, instructions) = parse_input(&input);

  let mut curr = (0, min_x_for_row(0, &map));
  let mut direction = Direction::Right;

  for ins in instructions {
    match ins {
      Instruction::Left(mag) => direction.walk(&mut curr, &map, mag),
      Instruction::Right(is_right) => direction = direction.turn(is_right),
    };
  }
  
  let ans = calc_score(&curr, &direction);
  dbg!(ans);
}

pub fn part2(input: String) {
  let (map, instructions) = parse_input(&input);

  let mut curr = (0, min_x_for_row(0, &map));
  let mut direction = Direction::Right;

  for ins in instructions {
    match ins {
      Instruction::Left(mag) => direction = direction.walk_cube(&mut curr, &map, mag),
      Instruction::Right(is_right) => direction = direction.turn(is_right),
    };
  }

  let ans = calc_score(&curr, &direction);
  dbg!(ans);
}
