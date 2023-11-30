#!/usr/bin/env python3

import argparse, sys
from abc import ABC
from ctypes import *
from runners import runners

class Solution(Structure):
  _fields_ = [
    ("s1", c_char_p),
    ("s2", c_char_p),
  ]

class AocLib(ABC):
  def __init__(self, name: str) -> None:
    self._lib = cdll.LoadLibrary(f"build/libaoc_{name}.so")
    self._run = self._lib.run
    self._run.argtypes = [c_char_p, c_uint, c_uint]
    self._run.restype = Solution

  def run(self, input: bytes, year: int, day: int) -> Solution:
    return self._run(input, year, day)
  
  def __enter__(self):
    return self
  
  def __exit__(self, exc_type, exc_value, traceback) -> None:
    pass
  
class AocAda(AocLib):
  def __init__(self) -> None:
    super().__init__("ada")

  def __enter__(self):
    self._lib.aoc_adainit()
    return super().__enter__()
  
  def __exit__(self, exc_type, exc_value, traceback) -> None:
    super().__exit__(exc_type, exc_value, traceback)
    self._lib.aoc_adafinal()

class AocNim(AocLib):
  def __init__(self) -> None:
    super().__init__("nim")
  
class AocZig(AocLib):
  def __init__(self) -> None:
    super().__init__("zig")

if __name__ == '__main__':
  parser = argparse.ArgumentParser(
    prog='aoc'
  )
  parser.add_argument('year', type=int)
  parser.add_argument('day', type=int)
  args = parser.parse_args()

  filename = f"input/{args.year}/day{args.day:02}.txt"
  try:
    with open(filename, 'rb') as f:
      input = f.read()

      runner = runners[args.year]
      if isinstance(runner, dict):
        runner = runner[args.day]
      match runner:
        case "ada": aoc_lib_class = AocAda
        case "nim": aoc_lib_class = AocNim
        case "zig": aoc_lib_class = AocZig
      with aoc_lib_class() as aoc_lib:
        solution = aoc_lib.run(input, args.year, args.day)
        print(solution.s1.decode(), solution.s2.decode(), sep="\n")
  except IOError:
    print("Cannot open input file (wrong year/day?)")
    sys.exit(1)
