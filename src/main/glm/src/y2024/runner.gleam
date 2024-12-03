import y2024/day01.{day01}

pub fn run(input: String, day: Int) -> #(String, String) {
  case day {
    1 -> day01(input)
    _ -> panic as "Invalid day for 2024"
  }
}
