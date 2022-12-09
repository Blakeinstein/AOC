use std::{collections::HashSet, cell::{RefCell}, rc::Rc};


#[derive(Debug)]
enum Direction {
  UP,
  DOWN,
  RIGHT,
  LEFT,
}

fn char_to_direction(c: &char) -> Direction {
  match c {
    'U' => Direction::UP,
    'D' => Direction::DOWN,
    'R' => Direction::RIGHT,
    'L' => Direction::LEFT,
    _ => panic!()
  }
}

#[derive(Debug)]
struct Move {
  direction: Direction,
  amt: i32,
}

fn parse_input(input: &str) -> Vec<Move> {
  input
    .split("\n")
    .filter(|s| !s.is_empty())
    .map(|line| Move{
      direction: char_to_direction(&line.chars().next().unwrap()),
      amt: line.split(" ").last().unwrap().parse().unwrap()
    })
    .collect()
}

#[derive(Hash, Clone, Debug, Copy, PartialEq, Eq)]
struct Point(i32, i32);

impl Point {
  fn move_self(&mut self, dir: &Direction) {
    match dir {
        Direction::UP => self.1 += 1,
        Direction::DOWN => self.1 -= 1,
        Direction::RIGHT => self.0 += 1,
        Direction::LEFT => self.0 -= 1,
    };
  }

  fn move_to(&mut self, target: &Point) {
    let dx = self.0 - target.0;
    let dy = self.1 - target.1;
    if dx.abs() <= 1 && dy.abs() <= 1 {
      return;
    }
    
    if dy != 0 {
      self.1 += if dy < 0 { 1 } else { -1 };
    }
    if dx != 0 {
      self.0 += if dx < 0 { 1 } else { -1 };
    }
  }
}

pub fn part1(input: String) { 
  let moves = parse_input(&input);
  let mut head = Point(0, 0);
  let mut tail = Point(0, 0);
  let mut locations = HashSet::new();
  locations.insert(tail);
  for step in moves {
    for i in 0..step.amt {
      head.move_self(&step.direction);
      tail.move_to(&head);
      locations.insert(tail);
    }
  }
  let ans = locations.len();
  dbg!(ans);
}

pub fn part2(input: String) {
  let moves = parse_input(&input);
  let mut tails = vec!();
  for i in 0..10 {
    tails.push(Rc::new(RefCell::new(Point(0, 0))));
  }
  let mut locations = HashSet::new();
  locations.insert(tails[9].borrow().clone());
  for step in moves {
    for i in 0..step.amt {
      tails[0].borrow_mut().move_self(&step.direction);
      for i in 1..10 {
        tails[i].borrow_mut().move_to(&tails[i-1].borrow());
      }
      locations.insert(tails[9].borrow().clone());
    }
  }
  let ans = locations.len();
  dbg!(ans);
}
