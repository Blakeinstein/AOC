#[derive(Debug, Clone, Copy)]
enum Cucumber {
    Empty,
    Down,
    Right,
}

impl Cucumber {
    fn from_char(c: char) -> Self {
        match c {
            '>' => Self::Right,
            'v' => Self::Down,
            _ => Self::Empty,
        }
    }
}

type Puzzle = Vec<Vec<Cucumber>>;

fn gen_input(input: &str) -> Puzzle {
    input
        .lines()
        .map(|line| line.chars().map(|pos| Cucumber::from_char(pos)).collect())
        .collect()
}

pub fn part1(input: String) {
    let mut puzzle = gen_input(&input);
    let h = puzzle.len();
    let w = puzzle[0].len();

    let mut part1 = 0;
    let mut previous_south = vec![false; w];
    let mut first_row_empty_spots = vec![false; w];
    let mut did_move = true;

    while did_move {
      did_move = false;
      part1 += 1;

      for c in 0..previous_south.len() {
        previous_south[c] = matches!(puzzle[0][c], Cucumber::Down);
      }

      for r in 0..h {
          let first = puzzle[r][0];
          let last = puzzle[r][w - 1];

          let mut c = w - 1;
          while c > 0 {
              if matches!(puzzle[r][c], Cucumber::Empty) && matches!(puzzle[r][c - 1], Cucumber::Right) {
                  puzzle[r][c - 1] = Cucumber::Empty;
                  puzzle[r][c] = Cucumber::Right;
                  did_move = true;
                  c -= 1;
              }

              c = c.saturating_sub(1);

              while c > 0 && !matches!(puzzle[r][c], Cucumber::Empty) {
                  c -= 1;
              }
          }

          if matches!(last, Cucumber::Right) && matches!(first, Cucumber::Empty) {
              puzzle[r][0] = Cucumber::Right;
              puzzle[r][w - 1] = Cucumber::Empty;
              did_move = true;
          }

          if r == 0 {
              for c in 0..w {
                  first_row_empty_spots[c] = matches!(puzzle[r][c], Cucumber::Empty);
              }
          }

          if r > 0 {
              for c in 0..puzzle[r].len() {
                  if matches!(puzzle[r][c], Cucumber::Empty) && previous_south[c] {
                      previous_south[c] = false;
                      puzzle[r][c] = Cucumber::Down;
                      puzzle[r - 1][c] = Cucumber::Empty;
                      did_move = true;
                  } else {
                      previous_south[c] = matches!(puzzle[r][c], Cucumber::Down);
                  }
              }
          }
      }

      for c in 0..previous_south.len() {
          if first_row_empty_spots[c] && previous_south[c] {
              puzzle[0][c] = Cucumber::Down;
              puzzle[h - 1][c] = Cucumber::Empty;
              did_move = true;
          }
      }
  }
  dbg!(part1);
}

pub fn part2(input: String) {
}
