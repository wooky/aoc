#!/usr/bin/env python3

import argparse, sys
from ctypes import *

class Solution(Structure):
  _fields_ = [
    ("s1", c_char_p),
    ("s2", c_char_p),
  ]

if __name__ == '__main__':
  parser = argparse.ArgumentParser(
    prog='aoc'
  )
  parser.add_argument('year', type=int)
  parser.add_argument('day', type=int)
  args = parser.parse_args()
  
  lib_zig = cdll.LoadLibrary('build/libaoc_zig.so')
  zig_run = lib_zig.run
  zig_run.argtypes = [c_char_p, c_uint, c_uint]
  zig_run.restype = Solution

  filename = f"input/{args.year}/day{args.day:02}.txt"
  try:
    with open(filename, 'rb') as f:
      input = f.read()
      solution = zig_run(input, args.year, args.day)
      print(solution.s1.decode(), solution.s2.decode(), sep="\n")
  except IOError:
    print("Cannot open input file (wrong year/day?)")
    sys.exit(1)
