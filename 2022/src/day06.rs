use std::collections::HashMap;

pub fn part1(input: String) { 
  let mut count_map: HashMap<char, usize> = HashMap::new();
  let markers = input.chars().collect::<Vec<_>>();

  for i in 0..4 {
    *count_map.entry(markers[i]).or_insert(0) += 1;
  }
  
  'main: for i in 4..markers.len() {
    *count_map.get_mut(&markers[i-4]).unwrap() -= 1;
    *count_map.entry(markers[i]).or_insert(0) += 1;
    for j in i-4..=i {
      if count_map.get(&markers[j]).unwrap_or(&0) > &1 {
        continue 'main;
      }
    }
    dbg!(i+1);
    break 'main;
  }
}

pub fn part2(input: String) {
  let mut count_map: HashMap<char, usize> = HashMap::new();
  let markers = input.chars().collect::<Vec<_>>();

  for i in 0..14 {
    *count_map.entry(markers[i]).or_insert(0) += 1;
  }
  
  'main: for i in 14..markers.len() {
    *count_map.get_mut(&markers[i-14]).unwrap() -= 1;
    *count_map.entry(markers[i]).or_insert(0) += 1;
    for j in i-14..=i {
      if count_map.get(&markers[j]).unwrap_or(&0) > &1 {
        continue 'main;
      }
    }
    dbg!(i+1);
    break 'main;
  }
}
