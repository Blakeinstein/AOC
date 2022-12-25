use std::{collections::{HashSet, VecDeque, HashMap}};

type Point = (i32, i32);

fn add_points(p1: &Point, p2: &Point) -> Point {
  (p1.0 + p2.0, p1.1 + p2.1)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Directions {
  N,
  NW,
  NE,
  E,
  W,
  S,
  SW,
  SE,
}

impl Directions {
  fn get_diff(&self) -> Point {
    match self {
      Self::N => (-1, 0),
      Self::NW => (-1, -1),
      Self::NE => (-1, 1),
      Self::E => (0, 1),
      Self::W => (0, -1),
      Self::S => (1, 0),
      Self::SW => (1, -1),
      Self::SE => (1, 1),
    }
  }
}

const ELF: char = '#';

type ElfMap = HashSet<Point>;

fn parse_input(input: &str) -> ElfMap {
  input
    .lines()
    .enumerate()
    .flat_map(
      |(i, line)| {
        line
          .trim()
          .chars()
          .enumerate()
          .filter(|(_, c)| *c == ELF)
          .map(move |(j, _)| (i as i32, j as i32))
      }
    )
    .collect()
}

type CycleKey = [Directions; 3];
type Cycle = VecDeque<CycleKey>;

fn get_elf_dir(
  elf_loc: &Point,
  elves: &ElfMap,
  cycle: &Cycle,
  scans: &HashMap<CycleKey, Directions>
) -> Option<Directions> {
  let mut pd = vec!();
  for scan in cycle.iter() {
    let mut occupied = false;
    for dir in scan.iter() {
      let diff = dir.get_diff();
      let adj = add_points(elf_loc, &diff);
      if elves.contains(&adj) {
        occupied = true;
        break;
      }
    }
    if !occupied {
      pd.push(scans.get(scan).unwrap());
    }
  }

  if 0 < pd.len() && pd.len() < 4 { Some(*pd[0]) } else { None }
}

type MoveList = HashMap<Point, Vec<Point>>;

fn get_new_pos(
  elf_loc: &Point,
  elves: &ElfMap,
  cycle: &Cycle,
  scans: &HashMap<CycleKey, Directions>,
) -> Point {
  let dir = get_elf_dir(elf_loc, elves, cycle, scans);
  match dir {
    Some(direction) => add_points(elf_loc, &direction.get_diff()),
    None => *elf_loc,
  }
}

fn get_all_moves(
  elves: &ElfMap,
  cycle: &Cycle,
  scans: &HashMap<CycleKey, Directions>,
) -> MoveList {
  elves.iter().fold(
    HashMap::new(),
    |mut acc, elf_loc| {
      let newp = get_new_pos(elf_loc, elves, cycle, scans);
      acc.entry(newp).or_insert_with(|| vec!()).push(*elf_loc);
      acc
    }
  )
}

fn move_elves(
  moves: &MoveList,
  elves: &ElfMap
) -> (ElfMap, bool) {
  let mut new = HashSet::new();
  for (key, val) in moves {
    if val.len() > 1 {
      for pos in val {
        new.insert(*pos);
      }
    } else {
      new.insert(*key);
    }
  }

  let did_not_change = new == *elves;
  (new, did_not_change)
}

fn box_elves(
  elves: &ElfMap,
) -> i32 {
  let mut top_corner = (i32::MAX, i32::MAX);
  let mut bottom_corner = (i32::MIN, i32::MIN);

  for (i, j) in elves {
    top_corner.0 = top_corner.0.min(*i);
    top_corner.1 = top_corner.1.min(*j);
    bottom_corner.0 = bottom_corner.0.max(*i);
    bottom_corner.1 = bottom_corner.1.max(*j);
  }

  return (
    (top_corner.0.abs() + (bottom_corner.0 + 1).abs()) *
    (top_corner.1.abs() + (bottom_corner.1 + 1).abs())
  ) - (elves.len() as i32)
}

pub fn part1(input: String) { 
  let mut elves = parse_input(&input);
  let scans = HashMap::from([
    ([Directions::N, Directions::NE, Directions::NW], Directions::N),
    ([Directions::S, Directions::SE, Directions::SW], Directions::S),
    ([Directions::W, Directions::NW, Directions::SW], Directions::W),
    ([Directions::E, Directions::NE, Directions::SE], Directions::E)
  ]);
  
  let mut cycle = VecDeque::from(
    [
      [Directions::N, Directions::NE, Directions::NW],
      [Directions::S, Directions::SE, Directions::SW],
      [Directions::W, Directions::NW, Directions::SW],
      [Directions::E, Directions::NE, Directions::SE]
    ]
  );
  
  for _ in 0..10 {
    let moves = get_all_moves(&elves, &cycle, &scans);
    (elves, _) = move_elves(&moves, &elves);
    cycle.rotate_left(1);
  }

  let ans = box_elves(&elves);
  dbg!(ans);
}

pub fn part2(input: String) {
  let mut elves = parse_input(&input);
  let scans = HashMap::from([
    ([Directions::N, Directions::NE, Directions::NW], Directions::N),
    ([Directions::S, Directions::SE, Directions::SW], Directions::S),
    ([Directions::W, Directions::NW, Directions::SW], Directions::W),
    ([Directions::E, Directions::NE, Directions::SE], Directions::E)
  ]);
  
  let mut cycle = VecDeque::from(
    [
      [Directions::N, Directions::NE, Directions::NW],
      [Directions::S, Directions::SE, Directions::SW],
      [Directions::W, Directions::NW, Directions::SW],
      [Directions::E, Directions::NE, Directions::SE]
    ]
  );

  let mut ans = 0;
  let mut no_change = false;
  while !no_change {
    let moves = get_all_moves(&elves, &cycle, &scans);
    (elves, no_change) = move_elves(&moves, &elves);
    ans += 1;
    cycle.rotate_left(1);
  }

  dbg!(ans);
}
