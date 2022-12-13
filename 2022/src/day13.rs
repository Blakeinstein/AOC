use std::iter::once;

use either::Either::{self, Right, Left};

#[derive(Debug, Clone)]
struct Packet(Either<usize,Vec<Packet>>);

fn make_packet(chars: &Vec<char>, idx: usize) -> (Packet, usize) {
  let mut packet: Vec<Packet> = vec!();
  let mut i = idx;
  while chars[i] != ']' {
    match chars[i] {
      '[' => {
        let (p, next) = make_packet(chars, i + 1);
        packet.push(p);
        i = next + 1;
      },
      ',' => {
        i += 1;
      },
      _ => {
        let mut num = String::from(chars[i]);
        i += 1;
        while chars[i].is_numeric() {
          num.push(chars[i]);
          i += 1;
        }
        packet.push(Packet(Left(num.parse().unwrap())));
      }
    }
  }
  return (Packet(Right(packet)), i);
}

fn parse_input(input: &str) -> Vec<(Packet, Packet)> {
  input
    .split("\n\n")
    .map(|packets| {
      let lines = packets.split("\n").collect::<Vec<_>>();
      let line1 = lines[0].chars().collect();
      let line2 = lines[1].chars().collect();
      return (make_packet(&line1, 1).0, make_packet(&line2, 1).0);
    })
    .collect()
}

fn compare_len(left: usize, right: usize) -> Option<bool> {
  let lp = Packet(Left(left));
  let rp = Packet(Left(right));
  return compare(&lp, &rp);
}

fn make_single_arr(val: usize) -> Packet {
  Packet(
    Right(
      Vec::from(
        [Packet(Left(val))]
      )
    )
  )
}

fn compare(left: &Packet, right: &Packet) -> Option<bool> {
  match (&left.0, &right.0) {
    (Left(l), Left(r)) => {
      if l == r {
        return None;
      }
      return Some(l < r);
    },
    (Right(l), Right(r)) => {
      let n = l.len().min(r.len());
      for i in 0..n {
        let next = compare(&l[i], &r[i]);
        if next.is_some()  {
          return next;
        }
      }
      return compare_len(l.len(), r.len());
    },
    (Left(l), Right(r)) => {
      let lp = make_single_arr(*l);
      return compare(&lp, right);
    },
    (Right(l), Left(r)) => {
      let rp = make_single_arr(*r);
      return compare(left, &rp);
    }
  };
}

pub fn part1(input: String) {
  let packets = parse_input(&input);
  let ans: usize = packets
    .iter()
    .enumerate()
    .filter(|(j,(p1, p2))| 
      compare(p1, p2).unwrap_or(false)
    )
    .map(|(idx, _)| idx + 1)
    .sum();
  dbg!(ans);
}

pub fn part2(input: String) {
  let packets = parse_input(&input);
  let all_packets = packets
    .iter()
    .flat_map(|tup| once(&tup.0).chain(once(&tup.1)))
    .collect::<Vec<_>>();
  
  let divider1 = make_single_arr(2);
  let divider2 = make_single_arr(6);

  let p1 = 1usize + all_packets
    .iter()
    .filter(|p| compare(p, &divider1).unwrap_or(false))
    .count();
  
  let p2 = 2usize + all_packets
    .iter()
    .filter(|p| compare(p, &divider2).unwrap_or(false))
    .count();

  let ans = p1 * p2;
  dbg!(ans);
}
