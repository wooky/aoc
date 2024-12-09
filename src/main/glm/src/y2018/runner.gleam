import y2018/day01.{day01}
import y2018/day02.{day02}
import y2018/day03.{day03}
import y2018/day04.{day04}
import y2018/day05.{day05}
import y2018/day06.{day06}
import y2018/day07.{day07}

pub fn run(input: String, day: Int) -> #(String, String) {
  case day {
    1 -> day01(input)
    2 -> day02(input)
    3 -> day03(input)
    4 -> day04(input)
    5 -> day05(input)
    6 -> day06(input)
    7 -> day07(input)
    _ -> panic as "Invalid day for 2018"
  }
}
