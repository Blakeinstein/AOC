use std::{collections::{HashMap, HashSet}, iter::Chain, slice::Iter};

use regex::Regex;

fn parse_input<'a>(input: &'a str) -> (
  HashMap<&'a str, usize>, 
  HashMap<&'a str, (usize, Vec<&'a str>)>
) {
  let re = Regex::new(
    r"^Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)$"
  ).unwrap();
  let mut keys = HashMap::new();
  let mut network = HashMap::new();
  input
  .lines()
  .enumerate()
  .for_each(
    |(i, line)| {
      let groups = re.captures(line).unwrap();
      let key_name = groups.get(1).unwrap().as_str();
      let key = keys.entry(key_name).or_insert(i);
      network.insert(
        key_name,
        (
          groups.get(2).unwrap().as_str().parse().unwrap(),
          groups.get(3).unwrap().as_str().split(", ").collect()
        )
      );
    }
  );
  (keys, network)
}

const START: &'static str = &"AA";

pub fn part1(input: String) { 
  let (keys, network) = parse_input(&input);
  
  const TOTAL_TIME: usize = 30;
  type Choice<'a> = (u32, usize, &'a str, HashMap<&'a str, bool>);
  let mut choices: HashMap<(usize, &str), Choice> = HashMap::new();
  let first_choice = (0, 0, START, HashMap::new());
  choices.insert((0, START), first_choice);
  
  for minute in 0..TOTAL_TIME {
    let mut new_choices = HashMap::new();
    for (_, choice) in &choices {
      let valid = &network.get(choice.2).unwrap().1;
      let new_time = choice.0 + 1;
      let mut new_release = choice.1;
      for v in valid {
        let new_valve = *v;
        let new_state = choice.3.clone();
        let new_choice = (new_time, new_release, new_valve, new_state);
        new_choices.insert((new_release, new_valve), new_choice);
      }
      let new_valve = choice.2;
      if !choice.3.contains_key(new_valve) {
        new_release += &network.get(choice.2).unwrap().0 * (TOTAL_TIME - minute - 1);
        let mut new_state = choice.3.clone();
        new_state.insert(new_valve, true);
        let new_choice = (new_time, new_release, new_valve, new_state);
        new_choices.insert((new_release, new_valve), new_choice);
      }
    }
    choices = new_choices;
  }
  
  let mut ans = 0;
  for (_, choice) in choices {
    if choice.1 > ans {
      ans = choice.1;
    }
  }
  
  dbg!(ans);
}

pub fn part2(input: String) {
  let (keys, network) = parse_input(&input);
  

  const TOTAL_TIME: usize = 26;
  type Choice<'a> = (usize, &'a str, HashSet<&'a str>, &'a str);
  // type Choice<'a> = (u32, usize, &'a str);
  let mut choices: HashMap<(usize, &str, &str), Choice> = HashMap::new();
  let first_choice = (0, START, HashSet::new(), START);
  choices.insert((0, START, START), first_choice);
  
  const TIME_DELTA: usize = 1;
  const OFFSET: usize = 0;
  
  // Simulate all possible choices
  'outer: for minute in OFFSET..TOTAL_TIME + OFFSET {
    println!("{} - choices {}", minute, choices.len());
    let mut new_choices = HashMap::new();
    for (_, choice) in &choices {
      let i_stay = [choice.1];
      let e_stay = [choice.3];
      let valid: Chain<Iter<_>, Iter<_>> = network.get(choice.1).unwrap().1.iter().chain(i_stay.iter());
      let valid_e: Chain<Iter<_>, Iter<_>> = network.get(choice.3).unwrap().1.iter().chain(e_stay.iter());
      if choice.2.len() == network.len() {
        println!("Skipped");
        break 'outer;
      }
      // Travel down a path
      for v in valid {
        let this_valid_e = valid_e.clone();
        for e in this_valid_e {
          let mut new_release = choice.0;
          let new_valve = *v;
          let new_e_valve = *e;
          let mut new_state = choice.2.clone();
          let new_choice;
          if new_e_valve == choice.3 && new_valve == choice.1 {
            if !new_state.contains(new_valve) || !choice.2.contains(new_e_valve) {
              new_release = choice.0;
              if !new_state.contains(new_valve) {
                new_release += &network.get(new_valve).unwrap().0 * (TOTAL_TIME - minute - TIME_DELTA);
                new_state.insert(new_valve);
              }
              if !new_state.contains(new_e_valve) {
                new_release += &network.get(new_e_valve).unwrap().0 * (TOTAL_TIME - minute - TIME_DELTA);
                new_state.insert(new_e_valve);
              }
            }
          }
          else if new_valve == choice.1 {
            new_release = choice.0;
            if !new_state.contains(new_valve) {
              new_release += &network.get(new_valve).unwrap().0 * (TOTAL_TIME - minute - TIME_DELTA);
              new_state.insert(new_valve);
            }
          }
          else if new_e_valve == choice.3 {
            new_release = choice.0;
            if !new_state.contains(new_e_valve) {
              new_release += &network.get(new_e_valve).unwrap().0 * (TOTAL_TIME - minute - TIME_DELTA);
              new_state.insert(new_e_valve);
            }
          }
          new_choice = (new_release, new_valve, new_state, new_e_valve);
          if !new_choices.contains_key(&(new_release, new_valve, new_e_valve)) && !new_choices.contains_key(&(new_release, new_e_valve, new_valve)) {
            new_choices.insert((new_release, new_valve, new_e_valve), new_choice);
          }
        }
      }
    }
    choices = new_choices;
  }
  
  let mut ans = 0;
  for (_, choice) in choices {
    if choice.0 > ans {
      ans = choice.0;
    }
  }
  dbg!(ans);
}
