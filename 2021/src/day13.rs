use std::collections::HashSet;
use std::cmp::max;

#[derive(Debug)]
struct Puzzle {
  dim: (usize, usize),
  paper: HashSet<(usize, usize)>,
  folds: Vec<(bool, usize)>,
}

fn get_input(input: &str) -> Puzzle {
  let mut paper = HashSet::new();
  let mut dim = (0, 0);
  let mut folds = vec!();
  let mut lines = input.lines();

  while let Some(point) = lines.next().unwrap().split_once(',') {
    let x = point.0.parse().unwrap();
    let y = point.1.parse().unwrap();
    paper.insert((x, y));
    dim.0 = max(dim.0, x);
    dim.1 = max(dim.1, y);
  }

  while let Some(fold) = lines.next() {
    let parts = fold.split_once('=').unwrap();
    folds.push((parts.0.chars().last().unwrap() == 'y', parts.1.parse().unwrap()))
  }

  Puzzle{dim, paper, folds}
}

fn fold_paper(paper: &mut HashSet<(usize, usize)>, pos: usize, hor: bool, dim: &mut (usize, usize)) {
  let mut new_points = vec!();
  paper.retain(|point| {
    if {
      if hor { point.1 > pos } else { point.0 > pos }
    } {
      let new_point = if hor { (point.0, 2 * pos - point.1) } else { (2 * pos - point.0, point.1) };
      new_points.push(new_point);
      false
    } else {
      true
    }
  });
  paper.extend(new_points);
  if hor {
    dim.1 = pos;
  } else {
    dim.0 = pos;
  }
}

pub fn part1(input: String) {
  let mut input = get_input(&input);
  let fold = input.folds[0];
  fold_paper(&mut input.paper, fold.1, fold.0, &mut input.dim);
  let part1 = input.paper.len();
  dbg!(part1);
}

pub fn part2(input: String) {
  let mut input = get_input(&input);
  for fold in input.folds {
    fold_paper(&mut input.paper, fold.1, fold.0, &mut input.dim);
  }
  for i in 0..input.dim.1 {
    for j in 0..input.dim.0 {
      print!("{}", if input.paper.contains(&(j, i)) { '#' } else { '.' });
    }
    println!();
  }
}