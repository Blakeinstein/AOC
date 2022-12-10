enum Instruction {
  NOOP,
  ADD(i32),
}

fn parse_instructions(input: &str) -> Vec<Instruction> {
  input
    .split("\n")
    .filter(|s| !s.is_empty())
    .map(|line| {
      if line.starts_with("noop") {
        return Instruction::NOOP;
      }
      return Instruction::ADD(line.split(" ").last().unwrap().parse().unwrap())
    })
    .collect()
}

pub fn part1(input: String) { 
  let instructions = parse_instructions(&input);
  let mut x = 1;
  let mut cycle = 20;
  let mut ans = 0;
  for instruction in instructions.iter() {
    let prev = x;
    let mut gain = 0;
    match instruction {
        Instruction::NOOP => {
          cycle += 1;
          if cycle % 40 == 0 {
            gain = x * (cycle - 20)
          }
        },
        Instruction::ADD(size) => {
          x += size;
          cycle += 2;
          gain = prev * (
            match cycle % 40 {
              0 => cycle - 20,
              1 => cycle - 21,
              _ => 0
            })
        },
    };
    ans += gain;
  }
  dbg!(ans);
}

fn get_pixel_char(cycle: usize, pos: i32) -> char {
  if (pos - (cycle % 40) as i32).abs() <= 1 {
    return '#';
  }
  ' '
}

pub fn part2(input: String) {
  let instructions = parse_instructions(&input);
  let mut pos = 1i32;
  let mut cycle = 0;
  let mut display = [' '; 240];
  for instruction in instructions.iter() {
    display[cycle] = get_pixel_char(cycle, pos);
    match instruction {
        Instruction::NOOP => {
          cycle += 1;
        },
        Instruction::ADD(size) => {
          display[cycle + 1] = get_pixel_char(cycle + 1, pos);
          pos += size;
          cycle += 2;
        },
    };
  }
  for i in 1..=240 {
    print!("{}", display[i-1]);
    if i % 40 == 0 {
      println!();
    }
  }
}
