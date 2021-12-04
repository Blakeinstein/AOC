type Board = [[(u32, bool); 5]; 5];

#[derive(Debug)]
struct Puzzle {
  draw_numbers: Vec<u32>,
  boards: Vec<Board>,
}

impl Puzzle {
  fn bingo(&mut self, start_index: usize) -> u32 {
    for i in start_index..self.draw_numbers.len()  {
      for board in self.boards.iter_mut() {
        for j in 0..5 {
          for k in 0..5 {
            if board[j][k].0 == self.draw_numbers[i] {
              board[j][k].1 = true;
            }
          }
        }
        if Self::check_bingo_board(board) {
          return self.draw_numbers[i] * Self::sum_unchecked(board);
        }
      }
    }
    0
  }

  fn check_bingo_board(board: &Board) -> bool {
    for i in 0..5 {
      let mut curr_row = true;
      let mut curr_col = true;
      for j in 0..5 {
        curr_row = curr_row & board[i][j].1;
        curr_col = curr_col & board[j][i].1;
      }
      if curr_col || curr_row {
        return true;
      }
    }
    false
  }

  fn sum_unchecked(board: &Board) -> u32 {
    let mut sum = 0;
    for i in 0..5 {
      for j in 0..5 {
        if !board[i][j].1 {
          sum += board[i][j].0;
        }
      }
    }
    return sum
  }

  fn bingo2(&mut self) -> u32 {
    for i in 0..self.draw_numbers.len()  {
      if self.boards.len() < 2 {
        return self.bingo(i);
      }
      for board in self.boards.iter_mut() {
        for j in 0..5 {
          for k in 0..5 {
            if board[j][k].0 == self.draw_numbers[i] {
              board[j][k].1 = true;
            }
          }
        }
      }
      self.boards.retain(|&board| !Self::check_bingo_board(&board));
    }
    0
  }
}

fn get_input(input: &str) -> Puzzle {
  let mut lines = input.lines();
  let draw_numbers: Vec<u32> = lines.next().unwrap().split(",").map(|num| num.parse::<u32>().unwrap()).collect();
  let mut boards = vec!();

  while lines.next().is_some() {
    let mut board = [[(0, false); 5]; 5];
    for i in 0..5 {
      let line = lines.next().unwrap().split_whitespace().map(|num| num.parse::<u32>().unwrap()).collect::<Vec<u32>>();
      for j in 0..5 {
        board[i][j] = (line[j], false);
      }
    }
    boards.push(board);
  }
  Puzzle { draw_numbers, boards }
}

pub fn part1(input: String) {
  let mut puzzle = get_input(&input);
  let result = puzzle.bingo(0);
  println!("Part1: {}", result);
}

pub fn part2(input: String) {
  let mut puzzle = get_input(&input);
  let result = puzzle.bingo2();
  println!("Part2: {}", result);
}