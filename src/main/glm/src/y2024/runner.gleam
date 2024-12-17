import y2024/day01.{day01}
import y2024/day02.{day02}
import y2024/day03.{day03}
import y2024/day04.{day04}
import y2024/day05.{day05}
import y2024/day06.{day06}
import y2024/day07.{day07}
import y2024/day08.{day08}
import y2024/day09.{day09}
import y2024/day10.{day10}
import y2024/day11.{day11}
import y2024/day12.{day12}

pub fn run(input: String, day: Int) -> #(String, String) {
  case day {
    1 -> day01(input)
    2 -> day02(input)
    3 -> day03(input)
    4 -> day04(input)
    5 -> day05(input)
    6 -> day06(input)
    7 -> day07(input)
    8 -> day08(input)
    9 -> day09(input)
    10 -> day10(input)
    11 -> day11(input)
    12 -> day12(input)
    _ -> panic as "Invalid day for 2024"
  }
}
