type Target = ((i32, i32), (i32, i32));

fn get_input(input: &str) -> Target {
  let (x_ranges, y_ranges) = input.lines().next().unwrap().split_once(": ").unwrap().1.split_once(", ").unwrap();
  let x_range = x_ranges.split_once("=").unwrap().1.split("..").map(|x| x.parse::<i32>().unwrap()).collect::<Vec<_>>();
  let y_range = y_ranges.split_once("=").unwrap().1.split("..").map(|x| x.parse::<i32>().unwrap()).collect::<Vec<_>>();
  (
    (x_range[0], x_range[1]),
    (y_range[0], y_range[1])
  )
}

fn n_sum(a: i32) -> i32 {
  a * (a + 1) / 2
}

fn y_max(target: &Target) -> i32 {
  target.1.0.abs() - 1
}

pub fn part1(input: String) {
  let target = get_input(&input);
  let part1 = n_sum(y_max(&target));
  dbg!(part1);
}

fn check_valid_velocity(init_x: i32, init_y: i32, target: &Target) -> bool{
  let mut px = 0;
  let mut py = 0;
  let mut x = init_x;
  let mut y = init_y;
  while px <= target.0.1 && py >= target.1.0 {
    px += x;
    py += y;
    x = 0.max(x - 1);
    y -= 1;
    if 
      target.0.0 <= px && px <= target.0.1 &&
      target.1.0 <= py && py <= target.1.1 {
      return true
    }
  }
  false
}

pub fn part2(input: String) {
  let target = get_input(&input);
  let x_max = target.0.1;
  let x_min = ((-1. + (1. + (8. * target.0.0 as f32).sqrt())) / 2.).ceil() as i32;
  let y_max = y_max(&target);
  let y_min = target.1.1 - x_max;
  let mut part2 = 0;
  for x in x_min..=x_max {
    for y in y_min..=y_max {
      if check_valid_velocity(x, y, &target) {
        part2 += 1
      }
    }
  }
  
  dbg!(part2);
}