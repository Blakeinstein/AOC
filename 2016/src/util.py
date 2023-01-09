from __future__ import annotations
from dataclasses import dataclass

@dataclass
class Point:
  x: float = 0
  y: float = 0
  z: float = 0
  
  def __mul__(self, other: int | float) -> Point:
    return Point(self.x * other, self.y * other, self.z * other)
  
  def __add__(self, other: Point) -> Point:
    return Point(self.x + other.x, self.y + other.y, self.z + other.z)
  
  def __hash__(self) -> int:
    return (hash(self.x) ^ (hash(self.y) << 1)) ^ hash(self.z)
  
  def __repr__(self) -> str:
    return f"x={self.x} y={self.y} z={self.z}"
  
  def yx(self) -> tuple[float, float]:
    return self.y, self.x
  
  def __abs__(self) -> float:
    return abs(self.x) + abs(self.y) + abs(self.z)
  
  def copy(self) -> Point:
    return Point(self.x, self.y, self.z)