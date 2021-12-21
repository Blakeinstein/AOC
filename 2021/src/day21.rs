use std::collections::HashMap;

fn get_start(input: &str) -> (usize, usize) {
  let mut lines = input.lines();
  (
    lines.next().unwrap().split_once(": ").unwrap().1.parse::<usize>().unwrap(),
    lines.next().unwrap().split_once(": ").unwrap().1.parse::<usize>().unwrap()
  )
}

pub fn part1(input: String) {
  let (p1, p2) = get_start(&input);
  let mut rolls = 0usize;
  let mut players: [(usize, usize); 2] = [(p1, 0), (p2, 0)];
  'game: loop {
    for player in players.iter_mut() {
      let steps = 3 * rolls + 6;
      rolls += 3;
      player.0 = (steps + player.0 - 1) % 10 + 1;
      player.1 += player.0;
      if player.1 >= 1000 {
        break 'game;
      }
    }
  }
  let part1 = rolls * players[0].1.min(players[1].1);
  dbg!(part1);
}

fn recursive_step(pos1: usize, pos2: usize, score1: usize, score2: usize, cache: &mut HashMap<(usize, usize, usize, usize), (i128, i128)>) -> (i128, i128) {
  let key = (pos1, pos2, score1, score2);
  if cache.contains_key(&key) {
    return cache[&(pos1, pos2, score1, score2)];
  } else {
    let ret;
    if score2 >= 21 {
      ret = (0, 1)
    } else {
      let rolls = [3, 4, 5, 4, 5, 6, 5, 6, 7, 4, 5, 6, 5, 6, 7, 6, 7, 8, 5, 6, 7, 6, 7, 8, 7, 8, 9];
      let mut wins1 = 0;
      let mut wins2 = 0;
      for next in rolls {
        let next_pos = (pos1 + next - 1) % 10 + 1;
        let next_score = score1 + next_pos;
        let rec = recursive_step(pos2, next_pos, score2, next_score, cache);
        wins1 += rec.1;
        wins2 += rec.0;
      }
      ret = (wins1, wins2)
    }
    cache.insert(key, ret);
    return ret;
  }
}

pub fn part2(input: String) {
  let (p1, p2) = get_start(&input);
  let mut cache = HashMap::new();
  let universes = recursive_step(p1, p2, 0, 0, &mut cache);
  let part2 = universes.0.max(universes.1);
  dbg!(part2);
}