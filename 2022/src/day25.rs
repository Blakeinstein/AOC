fn get_val(d: char) -> i64 {
  match d {
    '=' => -2,
    '-' => -1,
    _ => d.to_digit(10).unwrap() as i64
  }
}

fn parse_numbers(input: &str) -> Vec<i64> {
  input
    .lines()
    .map(
      |line| {
        let mut power = 1;
        let mut num = 0;
        for (i, digit) in line.trim().chars().rev().enumerate() {
          num += get_val(digit) * power;
          power *= 5;
        }
        num
      }
    )
    .collect()
}

fn make_snafu(num: i64) -> String {
  let mut ans = vec!();
  let mut remaining = num;
  while remaining > 0 {
    let next = remaining % 5;
    ans.push(match next {
        4 => '-',
        3 => '=',
        _ => char::from_digit(next as u32, 10).unwrap()
      }
    );
    if next > 2 {
      remaining += 5 - next;
    }
    remaining /= 5;
  }
  ans.iter().rev().collect()
}

pub fn part1(input: String) { 
  let nums = parse_numbers(&input);
  let ans = make_snafu(nums.iter().sum());
  dbg!(ans);
}
