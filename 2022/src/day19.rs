use std::{str::FromStr, num::ParseIntError, collections::{VecDeque, HashMap}};

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Material {
  Ore,
  Clay,
  Obsidian,
  Geode,
}

#[derive(Debug, Copy, Clone, Default)]
struct Cost {
  ores: usize,
  clay: usize,
  obsidian: usize,
}


#[derive(Debug, Copy, Clone, Default)]
struct Blueprint {
  ore_bot: Cost,
  clay_bot: Cost,
  geode_bot: Cost,
  obsidian_bot: Cost,
}


impl FromStr for Blueprint {
  type Err = ParseIntError;
  
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let re: Regex = Regex::new(r"Blueprint \d+: Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();
    let captures = re.captures(s).unwrap();
    let values: Vec<usize> = captures.iter().skip(1).map(|s| s.unwrap().as_str().parse().unwrap()).collect();
    Ok(Self {
      ore_bot: Cost {
        ores: values[0],
        ..Default::default()
      },
      clay_bot: Cost {
        ores: values[1],
        ..Default::default()
      },
      obsidian_bot: Cost {
        ores: values[2],
        clay: values[3],
        ..Default::default()
      },
      geode_bot: Cost {
        ores: values[4],
        obsidian: values[5],
        ..Default::default()
      },
    })
  }
}

impl Blueprint {
  fn cost(&self, material: Material) -> Cost {
    match material {
        Material::Ore => self.ore_bot,
        Material::Clay => self.clay_bot,
        Material::Obsidian => self.obsidian_bot,
        Material::Geode => self.geode_bot,
    }
  }

  fn can_build(&self, inv: &Inventory, material: Material) -> bool {
    let Cost {
      ores,
      clay,
      obsidian
    } = self.cost(material);

    inv.ores >= ores && inv.clay >= clay && inv.obsidian >= obsidian
  }

  fn should_build(&self, inv: &Inventory, material: Material, built: bool) -> bool {
    if material == Material::Geode {
      return true;
    }


    let max_cost = [
      self.ore_bot,
      self.clay_bot,
      self.obsidian_bot,
      self.geode_bot,
    ]
      .iter()
      .map(|c| {
        match material {
          Material::Ore => c.ores,
          Material::Clay => c.clay,
          Material::Obsidian => c.obsidian,
          Material::Geode => panic!(),
        }
      })
      .max()
      .unwrap_or(0);

    let still_needed = inv.robot_count(material) < max_cost;

    if !built {
      let prev_inv = inv.unmine();
      let skipped = self.can_build(&prev_inv, material);
      still_needed && !skipped
    } else {
      still_needed
    }
  }
}

#[derive(Default, Debug, Copy, Clone)]
struct Inventory {
  ore_bots: usize,
  clay_bots: usize,
  geode_bots: usize,
  obsidian_bots: usize,

  ores: usize,
  clay: usize,
  obsidian: usize,
  geodes: usize,
}

impl Inventory {
  fn new() -> Self {
    Self {
      ore_bots: 1,
      ..Default::default()
    }
  }

  fn robot_count(&self, material: Material) -> usize {
    match material {
        Material::Ore => self.ore_bots,
        Material::Clay => self.clay_bots,
        Material::Obsidian => self.obsidian_bots,
        Material::Geode => self.geode_bots,
    }
  }

  fn unmine(&self) -> Inventory {
    let mut other = self.clone();
    other.ores -= other.ore_bots;
    other.clay -= other.clay_bots;
    other.obsidian -= other.obsidian_bots;
    other.geodes -= other.geode_bots;
    other
  }

  fn _mine(&mut self) {
    self.ores += self.ore_bots;
    self.clay += self.clay_bots;
    self.obsidian += self.obsidian_bots;
    self.geodes +=  self.geode_bots;
  }

  fn mine(&self) -> Inventory {
    let mut other = self.clone();
    other._mine();
    other
  }

  fn build(&self, blueprint: &Blueprint, material: Material) -> Inventory {
    let mut other = self.clone();
    let Cost {
      ores,
      clay,
      obsidian,
    } = blueprint.cost(material);

    other.ores -= ores;
    other.clay -= clay;
    other.obsidian -= obsidian;

    other._mine();

    match material {
      Material::Ore => other.ore_bots += 1,
      Material::Clay => other.clay_bots += 1,
      Material::Obsidian => other.obsidian_bots += 1,
      Material::Geode => other.geode_bots += 1,
    }

    other
  }
}

fn search(factory: &Blueprint, minutes: i32) -> usize {
  let mut queue = VecDeque::new();
  queue.push_back((Inventory::new(), 0, false));

  let mut cache: HashMap<i32, usize> = HashMap::new();
  for i in 0..=minutes {
    cache.insert(i, 0);
  }

  while let Some((inv, min, built)) = queue.pop_front() {
    let &prev_best = cache.get(&min).unwrap();

    if (inv.geodes as i32) < (prev_best as i32 - 2) {
      continue;
    }
    cache.insert(min, prev_best.max(inv.geodes));

    if min == minutes {
      continue;
    }

    if factory.can_build(&inv, Material::Geode) {
      queue.push_back((inv.build(factory, Material::Geode), min + 1, true));
      continue;
    }

    queue.push_back((inv.mine(), min + 1, false));

    for material in [Material::Obsidian, Material::Clay, Material::Ore] {
      if factory.can_build(&inv, material) && factory.should_build(&inv, material, built) {
        queue.push_back((inv.build(factory, material), min + 1, true));
      }
    }
  }

  *cache.get(&minutes).unwrap()
}

fn parse_input(input: &str) -> Vec<Blueprint> {
  input
    .lines()
    .map(|s| s.parse().unwrap())
    .collect()
}

pub fn part1(input: String) { 
  let blueprints = parse_input(&input);

  let ans: usize = blueprints
    .iter()
    .enumerate()
    .map(|(i, bp)| {
      let score = search(bp, 24);
      score * (i + 1)
    })
    .sum();
  
  dbg!(ans);
}

pub fn part2(input: String) {
  let blueprints = parse_input(&input);

  let ans: usize = blueprints
    .iter()
    .take(3)
    .map(|bp| search(bp, 32))
    .product();
  
  dbg!(ans);
}
