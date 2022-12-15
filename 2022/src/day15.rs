use std::collections::HashSet;

use regex::Regex;

type Point = (i64, i64);


fn parse_input(input: &str) -> Vec<(Point, Point)> {
  let re = Regex::new("-?\\d+").unwrap();
  input
    .lines()
    .map(|line| {
      let a: Vec<i64> = re.find_iter(&line).map(|a| a.as_str().parse().unwrap()).collect();
      assert_eq!(a.len(), 4);
      return ((a[0], a[1]), (a[2], a[3]));
    })
    .collect()
}

pub fn part1(input: String) { 
  let pts = parse_input(&input);
  let bound = if pts.len() > 15 { 2000000 } else { 10 };
  let mut beacons = HashSet::new();
  
  for (st, ed) in pts.iter() {
    let r = (ed.1 - st.1).abs() + (ed.0 - st.0).abs();
    let d = r - (st.1 - bound).abs(); 
    if d < 0 {
      continue;
    }
    for i in st.0 - d ..= st.0 + d {
      beacons.insert(i);
    }
  }

  for (_, b) in pts.iter() {
    if b.1 == bound {
      beacons.remove(&b.0);
    }
  }

  let ans = beacons.len();
  dbg!(ans);
}

pub fn part2(input: String) {
  let pts = parse_input(&input);
  let bound = if pts.len() > 15 { 4000000 } else { 20 };

  let mut x = 0;
  let mut y = 0;
  'outer: while y <= bound {
      if bound < x {
          x = 0;
          y += 1;
      }
      for (st, ed) in pts.iter() {
          let r = (ed.1 - st.1).abs() + (ed.0 - st.0).abs();
          let d = r - (st.1 - y).abs();
          if d < 0 {
            continue;
          }
          let xf = st.0 - d;
          let xl = st.0 + d;
          if xf <= x && x <= xl {
              x = xl + 1;
              continue 'outer;
          }
      }
      let ans = x * 4000000 + y;
      dbg!(ans);
      return;
  }
}
