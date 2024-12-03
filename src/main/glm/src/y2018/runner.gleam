import y2018/day01.{day01}
import y2018/day02.{day02}

pub fn run(input: String, day: Int) -> #(String, String) {
  case day {
    1 -> day01(input)
    2 -> day02(input)
    _ -> panic as "Invalid day for 2018"
  }
}
