use std::{
  cmp::Reverse,
  collections::{BinaryHeap, HashMap, HashSet},
};

const MASK_LEFT: u8 = 0b0000_1000;
const MASK_RIGHT: u8 = 0b0000_0100;
const MASK_UP: u8 = 0b0000_0010;
const MASK_DOWN: u8 = 0b0000_0001;
const MASK_WALL: u8 = 0b1000_0000;

type Grid = Vec<Vec<u8>>;

fn parse_input(input: &str) -> (Grid, usize, usize) {
  (
    input
    .lines()
    .map(|line| {
      line.trim()
      .chars()
      .map(|c| match c {
        '<' => MASK_LEFT,
        '>' => MASK_RIGHT,
        '^' => MASK_UP,
        'v' => MASK_DOWN,
        '#' => MASK_WALL,
        '.' => 0,
        _ => panic!(),
      })
      .collect()
    })
    .collect(),
    1,
    input.lines().next().unwrap().len() - 2,
  )
}

fn next_state(grid: &Grid) -> Grid {
  assert!(grid.len() > 2);
  assert!(grid[0].len() > 2);
  
  let mut next = vec![vec![0; grid[0].len()]; grid.len()];
  let down = grid.len() - 2;
  let right = grid[0].len() - 2;
  
  for r in 0..grid.len() {
    assert_eq!(grid[r].len(), next[r].len());
    
    for c in 0..grid[r].len() {
      if grid[r][c] & MASK_WALL != 0 {
        next[r][c] = MASK_WALL;
        continue;
      }
      
      if grid[r][c] & MASK_LEFT != 0 {
        let col = if c == 1 { right } else { c - 1 };
        next[r][col] |= MASK_LEFT;
      }
      
      if grid[r][c] & MASK_RIGHT != 0 {
        let col = if c == right { 1 } else { c + 1 };
        next[r][col] |= MASK_RIGHT;
      }
      
      if grid[r][c] & MASK_UP != 0 {
        let row = if r == 1 { down } else { r - 1 };
        next[row][c] |= MASK_UP;
      }
      
      if grid[r][c] & MASK_DOWN != 0 {
        let row = if r == down { 1 } else { r + 1 };
        next[row][c] |= MASK_DOWN;
      }
    }
  }
  
  next
}

fn generate_all_states(initial: &Grid) -> (Vec<Grid>, usize) {
  let mut states = HashMap::new();
  let mut state = initial.to_vec();
  
  let cycle_start = loop {
    let next = next_state(&state);
    let order = states.len();
    
    let state_id = *states.entry(state).or_insert(order);
    if state_id != order {
      break state_id;
    }
    
    state = next;
  };
  
  let mut sequence = vec![vec![]; states.len()];
  for (state, idx) in states {
    sequence[idx] = state;
  }
  
  (sequence, cycle_start)
}

fn manhattan(from: (usize, usize), to: (usize, usize)) -> usize {
  from.0.abs_diff(to.0) + from.1.abs_diff(to.1)
}

const STEP: &[(isize, isize)] = &[(-1, 0), (0, -1), (0, 0), (0, 1), (1, 0)];

pub fn solve(
  states: &[Grid],
  cycle_start: usize,
  start_time: usize,
  rs: usize,
  cs: usize,
  re: usize,
  ce: usize,
) -> usize {
  let cycle_len = states.len() - cycle_start;
  
  let mut seen = HashSet::new();
  let mut pq = BinaryHeap::new();
  pq.push((
    Reverse(manhattan((rs, cs), (re, ce))),
    Reverse(start_time),
    (rs, cs),
  ));
  
  while let Some((Reverse(_cost), Reverse(time), (r, c))) = pq.pop() {
    if r == re && c == ce {
      return time;
    }
    
    let state_idx = if time < cycle_start {
      time + 1
    } else {
      cycle_start + (time + 1 - cycle_start) % cycle_len
    };
    
    let state = &states[state_idx];
    
    for (dr, dc) in STEP.iter().copied() {
      let Some(rx) = r.checked_add_signed(dr) else {
        continue;
      };
      let Some(cx) = c.checked_add_signed(dc) else {
        continue;
      };
      if rx >= state.len() || cx > state[rx].len() {
        continue;
      }
      
      if state[rx][cx] == 0 && seen.insert((state_idx, rx, cx)) {
        let cost = manhattan((rx, cx), (re, ce)) + time + 1;
        pq.push((Reverse(cost), Reverse(time + 1), (rx, cx)));
      }
    }
  }
  
  panic!("no solution")
}

pub fn part1(input: String) {
  let (grid, start, end) = parse_input(&input);
  let (states, cycle_start) = generate_all_states(&grid);
  let ans = solve(&states, cycle_start, 0, 0, start, grid.len() - 1, end);
  dbg!(ans);
}

pub fn part2(input: String) {
  let (grid, start, end) = parse_input(&input);
  let (states, cycle_start) = generate_all_states(&grid);
  let fwd = solve(&states, cycle_start, 0, 0, start, grid.len() - 1, end);
  let bck = solve(&states, cycle_start, fwd, grid.len() - 1, end, 0, start);
  let ans = solve(&states, cycle_start, bck, 0, start, grid.len() - 1, end);
  dbg!(ans);
}
