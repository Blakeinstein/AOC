use std::{collections::BinaryHeap, cell::RefCell, rc::Rc};

struct Operation {
  addition: bool,
  val: Option<u64>
}

impl Operation {
  fn op(&self, x: u64) -> u64 {
    if self.addition {
      x + self.val.unwrap_or(x)
    } else {
      x * self.val.unwrap_or(x)
    }
  }
  fn op_mod(&self, x: u64, modulo: u64) -> u64 {
    if self.addition {
      (x + self.val.unwrap_or(x)) % modulo
    } else {
      (x * self.val.unwrap_or(x)) % modulo
    }
  }
}

struct Test {
  val: u64,
  true_target: usize,
  false_target: usize,
}

impl Test {
  fn op(&self, x: u64) -> usize {
    if x % self.val == 0 {
      self.true_target
    } else {
      self.false_target
    }
  }
}

struct Monkey {
  items: Vec<u64>,
  operation: Operation,
  test: Test,
}

fn get_op(line: &str) -> Option<u64> {
  line.split(" ").last().unwrap().parse().ok()
}

fn make_operation(line: &str) -> Operation {
  Operation { 
    addition: line.contains("+"),
    val: get_op(line),
  }
}

fn make_test(lines: &[&str]) -> Test {
  Test {
    val: get_op(lines[0]).unwrap(),
    true_target: get_op(lines[1]).unwrap() as usize,
    false_target: get_op(lines[2]).unwrap() as usize,
  }
}

fn make_monkeys(input: &str) -> Vec<Rc<RefCell<Monkey>>> {
  input
    .split("\n\n")
    .map(|monkey| {
      let lines = monkey.split("\n").collect::<Vec<_>>();
      let items = lines[1].split(": ").last().unwrap()
      .split(", ").map(|num| num.parse().unwrap()).collect();
      let operation = make_operation(lines[2]);
      let test = make_test(&lines[3..]);
      Rc::new(RefCell::new(Monkey {
        items,
        operation,
        test,
      }))
    })
    .collect()
}

pub fn part1(input: String) { 
  let monkeys = make_monkeys(&input);
  let n = monkeys.len();
  let mut counts = vec![0; n];
  for _ in 0..20 {
    for i in 0..n {
      let mut monkey = monkeys[i].borrow_mut();
      counts[i] += monkey.items.len();
      monkey.items.iter().for_each(|item| {
        let mut worry = monkey.operation.op(*item);
        worry /= 3;
        monkeys[monkey.test.op(worry)].borrow_mut().items.push(worry);
      });
      monkey.items.clear();
    }
  }
  let ans: usize = BinaryHeap::from(counts).iter().take(2).product();
  dbg!(ans);
}

pub fn part2(input: String) {
  let monkeys = make_monkeys(&input);
  let n = monkeys.len();
  let mut counts = vec![0; n];
  let modulo = monkeys.iter().map(|x| x.borrow().test.val).product();
  for _ in 0..10000 {
    for i in 0..n {
      let mut monkey = monkeys[i].borrow_mut();
      counts[i] += monkey.items.len();
      monkey.items.iter().for_each(|item| {
        let worry = monkey.operation.op_mod(*item, modulo);
        monkeys[monkey.test.op(worry)].borrow_mut().items.push(worry);
      });
      monkey.items.clear();
    }
  }
  let ans: u128 = BinaryHeap::from(counts).iter().take(2).map(|x| *x as u128).product();
  dbg!(ans);
}
