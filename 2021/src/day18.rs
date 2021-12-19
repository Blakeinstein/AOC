#[derive(Clone)]
enum Pair {
  AnotherPair(Box<Pair>, Box<Pair>),
  Regular(i64),
  None
}


impl Pair {
  fn new(inp: String) -> Pair {
      if let Some(a) = inp.parse::<i64>().ok() {
          Pair::Regular(a)
      } else {
          let mut position = 0;
          let mut open = 0;
          for (i, ch) in inp.chars().enumerate() {
              match ch {
                  '[' => open += 1,
                  ']' => open -= 1,
                  ',' if open == 1 =>  position = i,
                  _ => {}
              }
          }
          Pair::new(inp[1..position].to_owned()) + Pair::new(inp[position+1..inp.chars().count()-1].to_owned())
      }
  }
  fn split(&mut self) -> bool {
      match self {
          Pair::AnotherPair(a, b) => {
              if !a.split() {
                  b.split()
              } else {
                  true
              }
          }
          Pair::Regular(a) => {
              if *a >= 10 {
                  *self = match std::mem::replace(self, Pair::None) {
                      Pair::Regular(a) => Pair::AnotherPair(Box::new(Pair::Regular(a/2)), Box::new(Pair::Regular(a/2 + a%2))),
                      v => v
                  };
                  true
              } else {
                  false
              }
          }
          Pair::None => panic!()
      }
  }
  fn adder(&mut self, from: bool, with: i64) {
      match self {
          Pair::AnotherPair(a, b) => {
              if from {
                  b.adder(from, with);
              } else {
                  a.adder(from, with);
              }
          }
          Pair::Regular(a) => *a += with,
          Pair::None => panic!()
      }
  }
  fn pangs(&mut self, depth: usize) -> Option<(i64, i64)> {
      match self {
          Pair::AnotherPair(a, b) => {
              if depth == 4 {
                  let left = a.pangs(depth + 1).unwrap().0;
                  let right = b.pangs(depth + 1).unwrap().0;
                  *self = match std::mem::replace(self, Pair::None) {
                      Pair::AnotherPair(_, _) => Pair::Regular(0),
                      v => v
                  };
                  Some((left, right))
              } else {
                  if let Some(a) = a.pangs(depth + 1) {
                      b.adder(false, a.1);
                      Some((a.0, 0))
                  } else if let Some(b) = b.pangs(depth + 1) {
                      a.adder(true, b.0);
                      Some((0, b.1))
                  } else {
                      None
                  }
              }
          }
          Pair::Regular(a) => {
              if depth > 4 {
                  Some((*a, *a))
              } else {
                  None
              }
          }
          _ => panic!()
      }
  }
  fn magnitude(&self) -> i64 {
      match self {
          Pair::AnotherPair(a, b) => 3 * a.magnitude() + 2 * b.magnitude(),
          Pair::Regular(a) => *a,
          Pair::None => panic!()    
      }
  }
}

use std::ops::Add;
impl Add for Pair {
  type Output = Self;
  fn add(self, other: Self) -> Self {
      let mut ret = match self {
          Pair::None => other,
          a => Pair::AnotherPair(Box::new(a), Box::new(other))
      };
      // tried turning this into a normal while loop. Didn't work. So have this monstrosity.
      while {
          while let Some(_) = ret.pangs(0) { }
          ret.split()
      } {}
      ret
  }
}

use std::fmt;
impl fmt::Debug for Pair {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
          Pair::AnotherPair(a, b) => {
              write!(f, "[")?;
              write!(f, "{:?}", a)?;
              write!(f, ", ")?;
              write!(f, "{:?}", b)?;
              write!(f, "]")
          }
          Pair::Regular(a) => {
              write!(f, "{:?}", a)
          }
          Pair::None => Ok(())
      }
  }
}


pub fn part1(input: String) {
  let part1 = input.lines().map(|x| Pair::new(x.to_string())).fold(Pair::None, |tot, x| tot + x).magnitude();
  dbg!(part1);
}

pub fn part2(input: String) {
  let pairs: Vec<Pair> = input.lines().map(|x| Pair::new(x.to_string())).collect();
  let part2 = (0..pairs.len())
    .map(|i| (0..pairs.len())
    .filter(|&j| j != i)
    .map(|j| (pairs[i].clone() + pairs[j].clone()).magnitude()).max().unwrap()).max().unwrap();

  dbg!(part2);
}