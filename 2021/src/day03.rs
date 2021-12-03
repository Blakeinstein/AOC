fn get_input(input: &str) -> Vec<[u8; 12]> {
  input.lines().map(|line| {
    let mut parts = line.chars();
    let mut out: [u8; 12] = [0; 12];
    for i in 0..12 {
      out[i] = if parts.next().unwrap() == '1' { 1 } else { 0 };
    }
    out
  }).collect()
}


pub fn part1(input: String) {
  let numbers = get_input(&input);
  let mut counts: [usize; 12] = [0; 12];
  for number in numbers.iter() {
    for i in 0..12 {
      if number[i] == 1 {
        counts[i] += 1;
      }
    }
  }
  let len = numbers.len();
  let mut gamma_arr: [char; 12] = ['0'; 12];
  let mut epsilon_arr: [char; 12] = ['0'; 12];

  for i in 0..12 {
    if counts[i] > len - counts[i] {
      gamma_arr[i] = '1';
    } else {
      epsilon_arr[i] = '1';
    }
  }

  let gamma = gamma_arr.iter().collect::<String>();
  let epsilon = epsilon_arr.iter().collect::<String>();

  println!("{}", isize::from_str_radix(&gamma, 2).unwrap() * isize::from_str_radix(&epsilon, 2).unwrap());
}

pub fn part2(input: String) {
  let mut s_o2 = get_input(&input);
  let mut s_co2 = s_o2.clone();

  for i in 0..12 {
    let len = s_o2.len();
    if len < 2 {
      break;
    }
    let count = s_o2.iter().filter(|&x| x[i] == 1).count();
    let curr_flag = (count >= len - count) as u8;
    s_o2.retain(|&x| x[i] == curr_flag);
  }

  for i in 0..12 {
    let len = s_co2.len();
    if len < 2 {
      break;
    }
    let count = s_co2.iter().filter(|&x| x[i] == 1).count();
    let curr_flag = (count < len - count) as u8;
    s_co2.retain(|&x| x[i] == curr_flag);
  }
  let o2 = s_o2.iter().next().unwrap().iter().map(|x| if *x == 1 { '1' } else { '0' }).collect::<String>();
  let co2 = s_co2.iter().next().unwrap().iter().map(|x| if *x == 1 { '1' } else { '0' }).collect::<String>();

  println!("{}", isize::from_str_radix(&o2, 2).unwrap() * isize::from_str_radix(&co2, 2).unwrap());
}