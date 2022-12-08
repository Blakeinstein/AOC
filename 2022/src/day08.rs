fn get_matrx(input: &str) -> Vec<Vec<i8>> {
  input
    .split("\n")
    .filter(|s| !s.is_empty())
    .map(|row| {
      row
        .chars()
        .map(|c| c.to_digit(10).unwrap() as i8)
        .collect()
    })
    .collect()
}

pub fn part1(input: String) { 
  let matrix = get_matrx(&input);
  let m = matrix.len();
  let n = matrix[0].len();
  let mut visible: Vec<Vec<bool>> = vec![vec![false; n]; m];
  for i in 0..m {
    let mut left_max = matrix[i][0] - 1;
    let mut right_max = matrix[i][n-1] - 1;
    for j in 0..n {
      let curr_left = matrix[i][j];
      let curr_right = matrix[i][n-j-1];

      // left and top visible
      if curr_left > left_max {
        visible[i][j] = true;
        left_max = curr_left;
      }

      if curr_right > right_max {
        visible[i][n-j-1] = true;
        right_max = curr_right;
      }
    }
  }
  for j in 0..n {
    let mut top_max = matrix[0][j] - 1;
    let mut bottom_max = matrix[m-1][j] - 1;
    for i in 0..m {
      let curr_top = matrix[i][j];
      let curr_bottom = matrix[m-i-1][j];

      // top and top visible
      if curr_top > top_max {
        visible[i][j] = true;
        top_max = curr_top;
      }

      if curr_bottom > bottom_max {
        visible[m-i-1][j] = true;
        bottom_max = curr_bottom;
      }
    }
  }
  let ans: usize = visible.iter().map(|row| row.iter().filter(|v| **v).count()).sum();
  dbg!(ans);
}

pub fn part2(input: String) {
  let matrix = get_matrx(&input);
  let m = matrix.len();
  let n = matrix[0].len();
  let mut score = 1;
  for i in 1..m-1 {
    for j in 1..n-1 {
      let mut curr_score = 1;
      curr_score *= i - (0..i).rev().find(|idx| matrix[*idx][j] >= matrix[i][j]).unwrap_or(0);
      curr_score *= (i+1..m).find(|idx| matrix[*idx][j] >= matrix[i][j]).unwrap_or(m-1) - i;
      curr_score *= j - (0..j).rev().find(|idx| matrix[i][*idx] >= matrix[i][j]).unwrap_or(0);
      curr_score *= (j+1..n).find(|idx| matrix[i][*idx] >= matrix[i][j]).unwrap_or(n-1) - j;
      score = score.max(curr_score);
    }
  }
  dbg!(score);
}
