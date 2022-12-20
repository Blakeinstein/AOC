#[derive(Debug, Copy, Clone, Default)]
struct ListNode {
  val: i64,
  left: usize,
  right: usize,
}

impl ListNode {
  fn new(val: i64) -> Self {
    Self {
      val,
      ..Default::default()
    }
  }
}

fn parse_input(input: &str) -> (Vec<ListNode>, usize) {
  let mut d: Vec<ListNode> = input
    .lines()
    .map(|v| ListNode::new(v.parse().unwrap()))
    .collect();

  let mut zero = 0;
  let n = d.len();
  
  d.iter_mut().enumerate().for_each(|(i, v)| {
    if v.val == 0 {
      zero = i;
    }
    v.left = i.checked_sub(1).unwrap_or(n-1);
    v.right = (i + 1) % n;
  });

  (d, zero)
}

fn wrap(idx: i64, n: i64) -> usize {
  (((idx % n ) + n) % n) as usize
}

fn mix(nums: &mut [ListNode]) {
  let n = nums.len();
  for i in 0..n {
    let curr = nums[i];
    let shift = wrap(curr.val, (n - 1) as i64);
    if shift == 0 {
      continue;
    }
    nums[curr.left].right = curr.right;
    nums[curr.right].left = curr.left;

    let mut node = curr.right;
    for i in 1..shift {
      node = nums[node].right;
    }
    
    let next = nums[node].right;
    let prev = node;

    nums[prev].right = i;
    nums[next].left = i;

    nums[i].left = prev;
    nums[i].right = next;
  }
}

fn get_ans(nums: &[ListNode], zero: usize) -> i64 {
  let mut pos = zero;
  let mut ans = 0;
  for round in 1..3001 {
    pos = nums[pos].right;
    if round % 1000 == 0 {
      ans += nums[pos].val;
    }
  }
  ans
}

pub fn part1(input: String) { 
  let (mut nums, zero) = parse_input(&input);
  mix(&mut nums);
  let ans = get_ans(&nums, zero);
  dbg!(ans);
}

const DECRYPTION_KEY: i64 = 811589153;

pub fn part2(input: String) {
  let (mut nums, zero) = parse_input(&input);
  for i in 0..nums.len() {
    nums[i].val *= DECRYPTION_KEY;
  }

  for _ in 0..10 {
    mix(&mut nums);
  }

  let ans = get_ans(&nums, zero);
  dbg!(ans);
}
