#[derive(PartialEq, Clone, Copy)]
enum Play {
  Rock,
  Paper,
  Scissor
}

fn get_play_from_char(choice: &str) -> Option<Play> {
  match choice {
    "A" | "X" => Some(Play::Rock),
    "B" | "Y" => Some(Play::Paper),
    "C" | "Z" => Some(Play::Scissor),
    _ => None
  }
}

fn get_score(choice: Play) -> u32 {
  match choice {
    Play::Rock => 1,
    Play::Paper => 2,
    Play::Scissor => 3,
  }
}

fn get_game_score(p1: Play, p2: Play) -> u32 {
  if p1 == p2 {
    return 3;
  }
  match (p1, p2) {
    (Play::Scissor, Play::Rock) | 
    (Play::Paper, Play::Scissor) |
    (Play::Rock, Play::Paper) => 6,
    (Play::Paper, Play::Rock) |
    (Play::Scissor, Play::Paper) |
    (Play::Rock, Play::Scissor) => 0,
    _ => 3,
  }
}

fn get_condition_score(condition: &str) -> u32 {
  match condition {
    "X" => 0,
    "Y" => 3,
    "Z" => 6,
    _ => 0
  }
}

fn get_correct_play(p: Play, condition: &str) -> Play {
  if condition == "Y" {
    return p;
  }
  match (p, condition) {
    (Play::Rock, "X") => Play::Scissor,
    (Play::Rock, "Z") => Play::Paper,
    (Play::Paper, "X") => Play::Rock,
    (Play::Paper, "Z") => Play::Scissor,
    (Play::Scissor, "X") => Play::Paper,
    (Play::Scissor, "Z") => Play::Rock,
    _ => p
  }
}

pub fn part1(input: String) {
  let score: u32 = input
    .split("\n")
    .filter(|x| !x.is_empty())
    .map(|game| {
      let plays: Vec<Play> = game
        .split(" ")
        .map(|x| get_play_from_char(x).unwrap())
        .collect();
      let mut score = get_score(plays[1]);
      score += get_game_score(plays[0], plays[1]);
      score
    }).sum();
  dbg!(score);
}

pub fn part2(input: String) { 
  let score: u32 = input
    .split("\n")
    .filter(|x| !x.is_empty())
    .map(|game| {
      let plays: Vec<&str> = game
        .split(" ")
        .collect();
      let play = get_play_from_char(plays[0]).unwrap();
      let condition = plays[1];
      let mut score = get_score(get_correct_play(play, condition));
      score += get_condition_score(condition);
      score
    }).sum();
  dbg!(score);
}