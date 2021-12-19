enum Instruction {
  Literal(u8, usize),
  Operator(u8, u8, Vec<Instruction>),
}

fn hex_to_bin(s: &str) -> Vec<u8> {
  s.chars()
    .fold(
      Vec::new(),
      |mut acc, c| {
        acc.extend(
          format!("{:0>4b}", c.to_digit(16).unwrap()).chars().map(|c| c.to_digit(2).unwrap() as u8)
        );
        acc
      }
    )
}

fn bits_to_num(bits: &[u8]) -> usize {
  let mut x = 0;
  for &b in bits {
    x = (x << 1) | b as usize
  }
  x
}


fn rec_parse(b: &[u8], max: usize) -> (usize, Vec<Instruction>) {
  let mut i = 0;
  let mut instructions = Vec::new();
  while i < b.len() && instructions.len() < max {
    let version = bits_to_num(&b[i..i+3]) as u8;
    i += 3;
    let id = bits_to_num(&b[i..i+3]);
    i += 3;
    match id {
      4 => {
        let mut val = 0;
        loop {
          val <<= 4;
          let x = bits_to_num(&b[i..i+5]);
          val += x & 0xf;
          i += 5;
          if x >> 4 == 0 {
            break;
          }
        }
        instructions.push(Instruction::Literal(version, val));
      }
      _ => {
        i += 1;
        let new_instruction = match b[i-1] {
          0 => {
            let nbits = bits_to_num(&b[i..i+15]);
            i += 15;
            let (_, next_instruction) = rec_parse(&b[i..i+nbits], usize::MAX);
            i += nbits;
            next_instruction
          }
          _ => {
            let ncounts = bits_to_num(&b[i..i+11]);
            i += 11;
            let (j, next_instruction) = rec_parse(&b[i..], ncounts);
            i += j;
            next_instruction
          }
        };
        instructions.push(Instruction::Operator(version, id as u8, new_instruction));
      }
    }
  }
  (i, instructions)
}

fn get_input(input: &str) -> Vec<Instruction> {
  let line = input.lines().next().unwrap();
  let b = hex_to_bin(line);
  rec_parse(&b, 1).1
}

fn version_sum(instruction: &Instruction) -> usize {
  match instruction {
    Instruction::Literal(version, _) => *version as usize,
    Instruction::Operator(version, _, ref instructions) => {
      *version as usize + instructions.iter().map(version_sum).sum::<usize>()
    }
  }
}

pub fn part1(input: String) {
  let input = get_input(&input);
  let part1 = version_sum(&input[0]);
  dbg!(part1);
}

fn eval(instruction: &Instruction) -> usize {
  match instruction {
    Instruction::Literal(_, val) => *val as usize,
    Instruction::Operator(_, id, ref instructions) => match id {
      0 => instructions.iter().map(eval).sum(),
      1 => instructions.iter().map(eval).product(),
      2 => instructions.iter().map(eval).min().unwrap(),
      3 => instructions.iter().map(eval).max().unwrap(),
      5 => (eval(&instructions[0]) > eval(&instructions[1])) as usize,
      6 => (eval(&instructions[0]) > eval(&instructions[1])) as usize,
      _ => (eval(&instructions[0]) == eval(&instructions[1])) as usize,
    }
  }
}

pub fn part2(input: String) {
  let input = get_input(&input);
  let part2 = eval(&input[0]);
  dbg!(part2);
}