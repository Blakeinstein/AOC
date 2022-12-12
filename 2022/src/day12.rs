use std::{collections::{HashSet, BinaryHeap, HashMap}, cmp::Ordering};

type MAP = Vec<Vec<char>>;
type POS = (usize, usize);

fn parse_input(input: &str) -> (MAP, POS, POS, Vec<POS>) {
  let mut map: MAP = input
    .split("\n")
    .filter(|s| !s.is_empty())
    .map(|c| c.chars().collect())
    .collect();
  let mut start = (0, 0);
  let mut end = (0, 0);
  let mut possible_starts = vec!();
  for i in 0..map.len() {
    for j in 0..map[i].len() {
      if map[i][j] == 'S' {
        start = (i, j);
        map[start.0][start.1] = 'a';
      }
      if map[i][j] == 'E' {
        end = (i, j);
        map[end.0][end.1] = 'z';
      } 
      if map[i][j] == 'a' {
        possible_starts.push((i, j));
      }
    }
  }
  (map, start, end, possible_starts)
}

const DIFFS: &'static [(i32, i32)] = &[(-1, 0), (1, 0), (0, -1), (0, 1)];

struct Vertex {
  point: POS,
  distance: usize,
}

impl Ord for Vertex {
  fn cmp(&self, other: &Self) -> Ordering {
      other.distance.cmp(&self.distance)
  }
}

impl PartialOrd for Vertex {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      Some(self.cmp(other))
  }
}

impl PartialEq for Vertex {
  fn eq(&self, other: &Self) -> bool {
      self.distance.eq(&other.distance)
  }
}

impl Eq for Vertex {}

fn shortest_path(map: &MAP, starts: &[POS], target: &POS) -> usize {
  let mut distances = HashMap::new();
  let mut visited = HashSet::new();
  let mut to_visit = BinaryHeap::new();
  for start in starts {
    distances.insert(*start, 0);
    to_visit.push(Vertex {
      point: *start,
      distance: 0
    });
  }
  while let Some(Vertex { point , distance }) = to_visit.pop() {
    if !visited.insert(point) {
      continue;
    }

    let curr_height = map[point.0][point.1] as i32;
    for (dx, dy) in DIFFS {
      let x = point.1 as i32 + dx;
      let y = point.0 as i32 + dy;
      if y >= 0 && y < map.len() as i32 && x >= 0 && x < map[0].len() as i32 {
        let neighbor = (y as usize, x as usize);
        if map[neighbor.0][neighbor.1] as i32 - curr_height <= 1 {
          let new_distance = distance + 1;
          let is_shorter = distances
            .get(&neighbor)
            .map_or(true, |&curr| new_distance < curr);

          if is_shorter {
            distances.insert(neighbor, new_distance);
            to_visit.push(Vertex {
              point: neighbor,
              distance: new_distance,
            })
          }
        }
      }
    }
  }

  return *distances.get(target).unwrap();
}

pub fn part1(input: String) { 
  let (map, start, end, _) = parse_input(&input);
  let ans = shortest_path(&map, &[start] ,&end);
  dbg!(ans);
}

pub fn part2(input: String) {
  let (map, _, end, possible_starts) = parse_input(&input);
  let ans = shortest_path(&map, &possible_starts, &end);
  dbg!(ans);
}
