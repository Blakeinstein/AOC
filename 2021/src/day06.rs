use std::collections::HashMap;

fn get_input(input: &str) -> Vec<i32> {
    input
        .lines().next().unwrap().split(',')
        .map(|x| x.parse::<i32>().unwrap())
        .collect()
}

fn solve(input: &mut Vec<i32>, reset: i32, new: i32, target: usize) -> usize {
  let mut m: HashMap<i32, usize> = HashMap::new();
  for i in 0..=new {
    m.insert(i, 0);
  }
  for x in input.iter() {
    m.insert(*x, m[x] + 1);
  }
  for _ in 0..target {
    // dbg!(&m);
    let temp = m[&0];
    for i in 1..=new {
      m.insert(i - 1, m[&i]);
    }
    m.insert(reset, m[&reset] + temp);
    m.insert(new, temp); 
  }
  m.values().sum::<usize>()
}

pub fn part1(input: String) {
  let mut input = get_input(&input);
  let part1 = solve(&mut input, 6, 8, 80);
  
  dbg!(part1);
}

pub fn part2(input: String) {
  let mut input = get_input(&input);
  let part2 = solve(&mut input, 6, 8, 256);
  
  dbg!(part2);
}