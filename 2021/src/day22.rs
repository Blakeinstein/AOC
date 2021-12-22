#[derive(Debug, Clone)]
struct Cuboid((i32, i32), (i32, i32), (i32, i32));

impl Cuboid {
    pub fn new(x1: i32, x2: i32, y1: i32, y2: i32, z1: i32, z2: i32) -> Self {
        Cuboid((x1, x2), (y1, y2), (z1, z2))
    }

    pub fn split_by(mut self, other: &Cuboid) -> Vec<Cuboid> {
        let mut ret = Vec::new();
        if (self.0.0 <= other.0.1 && self.0.1 >= other.0.0) && (self.1.0 <= other.1.1 && self.1.1 >= other.1.0)  && (self.2.0 <= other.2.1 && self.2.1 >= other.2.0) {
            // x-axis
            if self.0.0 < other.0.0 {
                ret.push(Cuboid::new(self.0.0, other.0.0-1, self.1.0, self.1.1, self.2.0, self.2.1));
                self.0.0 = other.0.0;
            }
            if self.0.1 > other.0.1 {
                ret.push(Cuboid::new(other.0.1+1, self.0.1, self.1.0, self.1.1, self.2.0, self.2.1));
                self.0.1 = other.0.1;
            }
            // y-axis
            if self.1.0 < other.1.0 {
                ret.push(Cuboid::new(self.0.0, self.0.1, self.1.0, other.1.0-1, self.2.0, self.2.1));
                self.1.0 = other.1.0;
            }
            if self.1.1 > other.1.1 {
                ret.push(Cuboid::new(self.0.0, self.0.1, other.1.1+1, self.1.1, self.2.0, self.2.1));
                self.1.1 = other.1.1;
            }
            // z-axis
            if self.2.0 < other.2.0 {
                ret.push(Cuboid::new(self.0.0, self.0.1, self.1.0, self.1.1, self.2.0, other.2.0-1));
                self.2.0 = other.2.0;
            }
            if self.2.1 > other.2.1 {
                ret.push(Cuboid::new(self.0.0, self.0.1, self.1.0, self.1.1, other.2.1+1, self.2.1));
                self.2.1 = other.2.1;
            }
        } else { ret.push(self)}
        ret
    }

    pub fn area(&self) -> u64 {
        let mut result = 1;
        result *= ((self.0.0 -self.0.1).abs()+1) as u64;
        result *= ((self.1.0 -self.1.1).abs()+1) as u64;
        result *= ((self.2.0 -self.2.1).abs()+1) as u64;
        result
    }

}

fn get_input(input: &str) -> Vec<(bool, Cuboid)> {
  input.lines().map(
    |line| {
      let (switch, region) = line.split_once(' ').unwrap();
      
      let points = region.split(',').map(
        |e| {
          let (_, range) = e.split_once('=').unwrap();
          let range = range.split("..").map(|e| e.parse::<i32>().unwrap()).collect::<Vec<i32>>();
          (range[0], range[1])
        }
      ).collect::<Vec<(i32, i32)>>();
      (switch == "on", Cuboid((points[0].0, points[0].1), (points[1].0, points[1].1), (points[2].0, points[2].1)))
    }
  ).collect()
}

fn solve(steps: &Vec<(bool, Cuboid)>, skip50: bool) -> u64 {
  let mut cuboids: Vec<Cuboid> = Vec::new();
  for step in steps {
    let mut new_cuboids = Vec::with_capacity(cuboids.len()+24) ;
    if skip50 && step.1.0.0.abs() > 50 {
      break;
    }
    for oc in cuboids {
      new_cuboids.append(&mut oc.split_by(&step.1));
    }
    if step.0 { 
      new_cuboids.push(step.1.clone());
    }
    cuboids = new_cuboids;
  }
  cuboids.iter().map(|c| c.area()).sum::<u64>()
}

pub fn part1(input: String) {
  let ins = get_input(&input);
  let part1 = solve(&ins, true);
  dbg!(part1);
}

pub fn part2(input: String) {
  let ins = get_input(&input);
  let part2 = solve(&ins, false);
  dbg!(part2);
}