use std::{collections::{HashMap}};

fn parse_input(input: &str) -> Vec<bool> {
  input
  .trim()
  .chars()
  .map(|x| x == '>')
  .collect()
}

const WIDTH: usize = 7;

const ROCK_TYPES: &'static [(&str, usize)] = &[
  ("hline", 1),
  ("cross", 3),
  ("L", 3),
  ("vline", 4),
  ("square", 2),
];

fn get_posns(h: i64, rock: &str) -> Vec<(i64, i64)> {
  match rock {
    "hline" => vec![(h + 3, 2), (h + 3, 3), (h + 3, 4), (h + 3, 5)],
    "cross" => vec![(h + 3, 3), (h + 4, 2), (h + 4, 3), (h + 4, 4), (h + 5, 3)],
    "L" => vec![(h + 3, 2), (h + 3, 3), (h + 3, 4), (h + 4, 4), (h + 5, 4)],
    "vline" => vec![(h + 3, 2), (h + 4, 2), (h + 5, 2), (h + 6, 2)],
    "square" => vec![(h + 3, 2), (h + 3, 3), (h + 4, 2), (h + 4, 3)],
    _ => panic!()
  }
}

fn attempt_move(
  wind: bool,
  rock_posns: &Vec<(i64, i64)>,
  situtation: &Vec<[i64; WIDTH]>
) -> Option<Vec<(i64, i64)>> {
  let mut new_posns = vec!();
  
  if wind {
    for p in rock_posns {
      if p.1 >= (WIDTH - 1) as i64 || situtation[p.0 as usize][(p.1 + 1) as usize] != 0 {
        return None;
      }
      new_posns.push((p.0, p.1 + 1));
    }
  } else {
    for p in rock_posns {
      if p.1 <= 0 || situtation[p.0 as usize][(p.1 - 1) as usize] != 0 {
        return None;
      }
      new_posns.push((p.0, p.1 - 1));
    }
  }

  return Some(new_posns);
}

fn fall_rock(
  rock_posns: &Vec<(i64, i64)>,
  situtation: &Vec<[i64; WIDTH]>
) -> Option<Vec<(i64, i64)>> {
  let mut new_posns = vec!();
  for p in rock_posns {
    if p.0 == 0 || situtation[(p.0 - 1) as usize][p.1 as usize] != 0 {
      return None;
    }
    new_posns.push((p.0 - 1, p.1));
  }
  return Some(new_posns);
}

pub fn part1(input: String) {
  let stream = parse_input(&input);
  let mut situation = vec![[0; WIDTH]; 4];
  let mut curr_highest = -1;
  let rock_names = ROCK_TYPES.iter().map(|(name, _)| name).collect::<Vec<_>>();
  let rock_map: HashMap<_, _> = ROCK_TYPES.to_vec().into_iter().collect();
  let mut move_idx = 0;
  for rock_num in 0..2022 {
    let curr_rock = rock_names[rock_num % 5];
    let rock_height = rock_map.get(curr_rock).unwrap();
    let mut curr_posns = get_posns(curr_highest + 1, curr_rock);
    for _ in 0..*rock_height {
      situation.push([0; WIDTH]);
    }
    loop {
      if move_idx >= stream.len() {
        move_idx -= stream.len();
      }
      if let Some(next_pos) = attempt_move(
        stream[move_idx],
        &curr_posns, 
        &situation
      ) {
        curr_posns = next_pos;
      }
      move_idx += 1;
      let next_pos = fall_rock(&curr_posns, &situation);
      if next_pos.is_none() {
        for p in curr_posns {
          situation[p.0 as usize][p.1 as usize] = 1;
          curr_highest = curr_highest.max(p.0);
        }
        break;
      }
      curr_posns = next_pos.unwrap();
    }
  }

  let ans = curr_highest + 1;
  dbg!(ans);
}

fn make_bitset(bits: &[i64; WIDTH]) -> usize {
  let mut ans = 0;
  for i in 0..WIDTH {
    ans = (ans << 1) + i;
  }
  return ans;
}

pub fn part2(input: String) {
  let stream = parse_input(&input);
  let mut situation = vec![[0; WIDTH]; 4];
  let mut curr_highest = -1;
  let rock_names = ROCK_TYPES.iter().map(|(name, _)| name).collect::<Vec<_>>();
  let rock_map: HashMap<_, _> = ROCK_TYPES.to_vec().into_iter().collect();
  let mut move_idx = 0;
  let mut seven_peaks = [0; WIDTH];
  let mut prev_seen = HashMap::new();
  let mut h_dict = HashMap::new();
  for rock_num in 0..1000000000000 {
    let curr_rock = rock_names[rock_num % 5];
    let rock_height = rock_map.get(curr_rock).unwrap();
    let mut curr_posns = get_posns(curr_highest + 1, curr_rock);
    for _ in 0..*rock_height {
      situation.push([0; WIDTH]);
    }
    loop {
      if move_idx >= stream.len() {
        move_idx -= stream.len();
      }
      if let Some(next_pos) = attempt_move(
        stream[move_idx],
        &curr_posns, 
        &situation
      ) {
        curr_posns = next_pos;
      }
      move_idx += 1;
      let next_pos = fall_rock(&curr_posns, &situation);
      if next_pos.is_none() {
        let old_max = curr_highest;
        for p in curr_posns.iter() {
          situation[p.0 as usize][p.1 as usize] = 1;
          curr_highest = curr_highest.max(p.0);
        }
        let max_change = curr_highest - old_max;
        for idx in 0..WIDTH {
          seven_peaks[idx] -= max_change; 
        }
        for p in curr_posns.iter() {
          let this_peak = p.0 - curr_highest;
          seven_peaks[p.1 as usize] = seven_peaks[p.1 as usize].max(this_peak);
        }
        break;
      }
      curr_posns = next_pos.unwrap();
    }
    h_dict.insert(rock_num, curr_highest);
    let k = (make_bitset(&seven_peaks), move_idx, rock_num % 5);
    if let Some(prev) =  prev_seen.get(&k) {
      if *prev != 0 {
        let then_count = prev;
        let highest_then = h_dict[&prev];
        let highest_now = curr_highest;
        let height_change = highest_now - highest_then;
        let cycle_size = rock_num - prev;
        let goal = 1000000000000 - prev;
        let num_cycles = goal / cycle_size;
        let left_over = goal % cycle_size;
        let leftover_height = h_dict[&(then_count + left_over)] - highest_then;
        let ans = highest_then + leftover_height + (num_cycles as i64 * height_change);
        dbg!(ans);
        return;
      }
    }
    prev_seen.insert(k, rock_num);
  }
  let ans = curr_highest + 1;
  dbg!(ans);
}
