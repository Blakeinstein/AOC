#!/bin/bash
x=0

while [ $x -lt 25 ]; do
  echo $x
  printf -v fname "src/day%02d.rs" "$((++x))"
  echo "pub fn part1(input: String) { 
  todo!();
}

pub fn part2(input: String) {
  todo!();
}" > $fname
done
