import y2024/day01.{day01}
import y2024/day02.{day02}
import y2024/day03.{day03}
import y2024/day04.{day04}

pub fn run(input: String, day: Int) -> #(String, String) {
  case day {
    1 -> day01(input)
    2 -> day02(input)
    3 -> day03(input)
    4 -> day04(input)
    _ -> panic as "Invalid day for 2024"
  }
}
