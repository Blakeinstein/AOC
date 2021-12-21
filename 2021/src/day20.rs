use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
enum Pixel {
  Dark = 0,
  Light = 1,
}

impl Pixel {
  fn from_char(c: char) -> Option<Self> {
    match c {
      '#' => Some(Pixel::Light),
      '.' => Some(Pixel::Dark),
      _ => None
    }
  }
}

type ImageLookup = HashMap<(i32, i32), Pixel>;

struct Image {
  lookup: ImageLookup,
  offset: i32,
  dim: (usize, usize)
}

struct Puzzle {
  algo: Vec<Pixel>,
  image: Image
}

fn get_input(input: &str) -> Puzzle {
  let mut lines = input.lines();
  let algo = lines.next().unwrap().chars().map(|point| Pixel::from_char(point).unwrap()).collect();
  let mut dim = (0, 0);
  let lookup = lines.skip(1).enumerate()
  .flat_map(|(i, line)|
    line
    .chars()
    .enumerate()
    .map(|(j, c)| {
      dim.0 = dim.0.max(i);
      dim.1 = dim.1.max(j);
      ((i as i32, j as i32), Pixel::from_char(c).unwrap())
    })
    .collect::<Vec<_>>()
  ).collect::<ImageLookup>();
  let image = Image { lookup, offset: -1, dim };
  Puzzle { algo, image }
}

fn edge(algo: &Vec<Pixel>, depth: usize) -> isize {
  match algo[0] {
    Pixel::Dark => 0isize,
    Pixel::Light => (depth & 1) as isize
  }
}

fn enhance(image: &mut Image, algo: &Vec<Pixel>, edge: isize) {
  let (i, j) = image.dim;
  let offset = image.offset;
  let mut lookup_next = ImageLookup::new();
  for i in offset..=(i as i32 - offset) {
    for j in offset..=(j as i32 - offset) {
      let mut value = 0;
      for dx in [-1, 0, 1] {
        for dy in [-1, 0, 1] {
          let next = image.lookup.get(&(i + dx, j + dy)).map_or(edge, |&val| (val as isize));
          value = (value << 1) | next;
        }
      }
      lookup_next.insert((i, j), *algo.get(value as usize).unwrap());
    }
  }
  image.lookup = lookup_next;
  image.offset = offset - 1;
}

fn step(puzzle: &mut Puzzle, count: usize) -> usize {
  for depth in 0..count {
    let border = edge(&puzzle.algo, depth);
    enhance(&mut puzzle.image, &puzzle.algo, border);
  }
  puzzle.image.lookup.values().filter(|&v| matches!(*v, Pixel::Light)).count()
}

pub fn part1(input: String) {
  let mut input = get_input(&input);
  let part1 = step(&mut input, 2);
  dbg!(part1);
}

pub fn part2(input: String) {
  let mut input = get_input(&input);
  let part2 = step(&mut input, 50);
  dbg!(part2);
}