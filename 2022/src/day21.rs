use std::{rc::Rc, cell::RefCell, collections::{HashMap}, ops::{Mul, Add, Sub, Div}};

use either::Either;

#[derive(Debug, Clone, Copy)]
struct Operation<'a> {
  l_op: &'a str,
  r_op: &'a str,
  op: fn(f64, f64) -> f64,
  op_char: char,
  has_human: bool,
}

type Monkey<'a> = Either<f64, Operation<'a>>;

type MonkeySet<'a> = HashMap<&'a str, Rc<RefCell<Monkey<'a>>>>;

fn parse_input<'a>(input: &'a str) -> MonkeySet {
  input
    .lines()
    .map(|line| {
      let parts = line
        .split(&[':', ' '][..])
        .collect::<Vec<_>>();
      let key = parts[0];
      if parts.len() == 3 {
        let val = parts[2].parse().unwrap();
        return (
          key,
          Rc::new(
            RefCell::new(
              Monkey::Left(val)
            )
          )
        );
      }
      let op = match parts[3] {
        "*" => f64::mul,
        "+" => f64::add,
        "-" => f64::sub,
        "/" => f64::div,
        _ => panic!()
      };

      (
        key,
        Rc::new(
          RefCell::new(
            Monkey::Right(
              Operation {
                l_op: parts[2],
                r_op: parts[4],
                op,
                has_human: key == "humn",
                op_char: parts[3].chars().next().unwrap(),
              }
            )
          )
        )
      )
    })
    .collect()
}

fn eval(monkeys: &MonkeySet, key: &str) -> f64 {
  let node = monkeys.get(key).unwrap().clone();
  let mut val = node.borrow_mut();
  if let Some(oper) = val.right() {
    let ans = (oper.op)(eval(monkeys, oper.l_op), eval(monkeys, oper.r_op));
    *val = Monkey::Left(ans);
  }
  val.unwrap_left()
}

pub fn part1(input: String) { 
  let monkeys = parse_input(&input);
  let ans = eval(&monkeys, "root") as u64;
  dbg!(ans);
}

fn has_human(monkeys: &MonkeySet, key: &str) -> bool {
  if key == "humn" {
    return true;
  }
  let node = monkeys.get(key).unwrap().clone();
  let mut val = node.borrow_mut();
  if val.is_left() {
    return false;
  }
  let mut oper = val.as_mut().unwrap_right();
  if !oper.has_human {
    oper.has_human = has_human(monkeys, oper.l_op) || has_human(monkeys, oper.r_op)
  };
  // dbg!(key, oper.has_human);
  return oper.has_human;
}

fn reduce(monkeys: &MonkeySet, key: &str) {
  let node = monkeys.get(key).unwrap().clone();
  let val = node.borrow_mut();
  if val.is_left() {
    return;
  }
  let oper = val.unwrap_right();
  if !has_human(monkeys, oper.l_op) {
    reduce(monkeys, oper.l_op);
  }
  if !has_human(monkeys, oper.r_op) {
    reduce(monkeys, oper.r_op);
  }
}

fn walk(monkeys: &MonkeySet, key: &str) -> String {
  if key == "humn" {
    return String::from("x");
  }
  let node = monkeys.get(key).unwrap().clone();
  let val = node.borrow();
  if let Some(v) = val.left() {
    return format!("{}", v);
  }

  let oper = val.unwrap_right();

  return format!(
    "({}{}{})",
    walk(monkeys, oper.l_op),
    oper.op_char,
    walk(monkeys, oper.r_op)
  );  
}

pub fn part2(input: String) {
  let monkeys = parse_input(&input);
  let eq = monkeys.get("root").unwrap().borrow().unwrap_right();
  
  reduce(&monkeys, eq.l_op);
  reduce(&monkeys, eq.r_op);
  let (key, known_val) = if has_human(&monkeys, eq.l_op) {
    (eq.l_op, eval(&monkeys, eq.r_op))
  } else {
    (eq.r_op, eval(&monkeys, eq.l_op))
  };
  
  println!("{}={}", walk(&monkeys, eq.l_op), known_val);
}
