use std::ops::{Add, AddAssign, Sub, SubAssign};

enum Directions {
  Forward,
  Down,
  Up
}

impl Directions {
  fn from_char(direction: &str) -> Directions {
    match direction {
      "up" => Directions::Up,
      "down" => Directions::Down,
      "forward" => Directions::Forward,
      _ => panic!("Unknown direction: {}", direction)
    }
  }
}


struct Operation {
  direction: Directions,
  distance: i32
}

fn get_input(input: &str) -> Vec<Operation> {
  input.lines().map(|line| {
    let mut parts = line.split_whitespace();
    let direction = Directions::from_char(parts.next().unwrap());
    let distance = parts.next().unwrap().parse::<i32>().unwrap();
    Operation { direction, distance }
  }).collect()
}


#[derive(Debug, Default, Copy, Clone, Eq, Hash, PartialEq)]
struct Position(i32, i32);

impl Add for Position {
  type Output = Self;
  fn add(self, rhs: Self) -> Self::Output {
      Self(self.0 + rhs.0, self.1 + rhs.1)
  }
}

impl Sub for Position {
  type Output = Self;
  fn sub(self, rhs: Self) -> Self::Output {
      Self(self.0 - rhs.0, self.1 - rhs.1)
  }
}

impl AddAssign for Position {
  #[inline(always)]
  fn add_assign(&mut self, other: Self) {
      *self = *self + other
  }
}

impl SubAssign for Position {
  fn sub_assign(&mut self, other: Self) {
      *self = *self - other
  }
}

impl Position {
  fn move_direction(&mut self, dir: Directions, mag: i32) {
    match dir {
      Directions::Forward => *self += Position(mag, 0),
      Directions::Down => *self += Position(0, mag),
      Directions::Up => *self += Position(0, -mag)
    };
  }
}


pub fn part1(input: String) {
  let ops = get_input(&input);
  let mut pos = Position(0, 0);
  for op in ops {
    pos.move_direction(op.direction, op.distance);
  }
  println!("{}", pos.0 * pos.1);
}

struct Aim {
  pos: Position,
  aim: i32
}

impl Aim {
  fn move_direction(&mut self, dir: Directions, mag: i32) {
    match dir {
      Directions::Forward => self.pos += Position(mag, mag * self.aim),
      Directions::Down => self.aim += mag,
      Directions::Up => self.aim -= mag
    };
  }
}

pub fn part2(input: String) {
  let ops = get_input(&input);
  let mut aim = Aim{
    pos: Position(0, 0),
    aim: 0
  };
  for op in ops {
    aim.move_direction(op.direction, op.distance);
  }
  println!("{}", aim.pos.0 * aim.pos.1);
}