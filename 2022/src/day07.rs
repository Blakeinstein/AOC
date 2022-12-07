use std::{collections::HashMap, rc::Rc, cell::RefCell};

struct Node<'a> {
  parent: Option<Rc<RefCell<Node<'a>>>>,
  size: Option<u64>,
  children: Option<HashMap<&'a str, Rc<RefCell<Node<'a>>>>>
}

fn get_file_tree<'a>(input: &'a str) -> Rc<RefCell<Node<'a>>> {
  let root_node = Rc::new(RefCell::new(
    Node {
      parent: None,
      size: None,
      children: Some(HashMap::new())
    }
  ));

  let mut curr_node = root_node.clone();

  input
    .split("\n")
    .filter(|s| !s.is_empty())
    .for_each(|line| {
      if line.starts_with("$ cd") {
        let path = line.split(" ").last().unwrap();
        if path == "/" {
          curr_node = root_node.clone();
          return;
        }
        if path == ".." {
          let current = curr_node.clone();
          let borrow = current.borrow();
          let next = borrow.parent.as_ref().unwrap();
          curr_node = next.clone();
          return;
        }
        let current = curr_node.clone();
        let next = current
          .borrow_mut()
          .children
          .as_mut()
          .unwrap()
          .entry(&path)
          .or_insert_with(|| {
            Rc::new(RefCell::new(
              Node {
                parent: Some(curr_node.clone()),
                size: None,
                children: Some(HashMap::new())
              }
            ))
          })
          .clone();
        curr_node = next;
      } else if line.starts_with("dir") {
        let path = line.split(" ").last().unwrap();
        curr_node
          .borrow_mut()
          .children
          .as_mut()
          .unwrap()
          .entry(&path)
          .or_insert_with(|| {
            Rc::new(RefCell::new(
              Node {
                parent: Some(curr_node.clone()),
                size: None,
                children: Some(HashMap::new())
              }
            ))
        });
      } else if !line.starts_with("$ ls") {
        let mut iter = line.split(" ");
        let file_size = iter.next().unwrap().parse::<u64>().unwrap();
        let file_name = iter.next().unwrap();
        curr_node
          .borrow_mut()
          .children
          .as_mut()
          .unwrap()
          .entry(&file_name)
          .or_insert_with(|| {
            Rc::new(RefCell::new(
              Node {
                parent: Some(curr_node.clone()),
                size: Some(file_size),
                children: None
              }
            ))
        });
      }
    });
  
  return root_node;
}

fn calc_sizes(node: Rc<RefCell<Node>>) -> u64 {
  if node.borrow().size.is_none() {
    let mut size = 0;
    for entry in node.borrow().children.as_ref().unwrap().values() {
      size += calc_sizes(entry.clone());
    }
    node.borrow_mut().size = Some(size);
    return size;
  }
  return node.borrow().size.unwrap();
}

const FILTER: u64 = 100000;

fn calc_p1(node: Rc<RefCell<Node>>) -> u64 {
  if node.borrow().children.is_none() {
    return 0;
  }
  let curr = node.borrow().size.unwrap();
  let mut sum = 0;
  if curr < FILTER {
    sum += curr;
  }
  sum += node
    .borrow()
    .children
    .as_ref()
    .unwrap()
    .values()
    .map(|n| calc_p1(n.clone()))
    .sum::<u64>();
  return sum;
}

pub fn part1(input: String) {
  let root_node = get_file_tree(&input);
  let root_size = calc_sizes(root_node.clone());

  let ans = calc_p1(root_node.clone());
  
  dbg!(ans);
}

const MAX_SPACE: u64 = 70000000;
const REQUIRED_SPACE: u64 = 30000000;

fn calc_p2(node: Rc<RefCell<Node>>, req_free_space: u64) -> u64 {
  let curr = node.borrow().size.unwrap();

  if node.borrow().children.is_none() || curr < req_free_space {
    return MAX_SPACE;
  }

  return curr.min(
    node
      .borrow()
      .children
      .as_ref()
      .unwrap()
      .values()
      .map(|n| calc_p2(n.clone(), req_free_space))
      .min()
      .unwrap()
    );
}

pub fn part2(input: String) {
  let root_node = get_file_tree(&input);
  let root_size = calc_sizes(root_node.clone());

  let ans = calc_p2(root_node.clone(), REQUIRED_SPACE - (MAX_SPACE - root_size) );
  
  dbg!(ans);
}
