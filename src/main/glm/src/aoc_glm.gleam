import gleam/bit_array
import gleam/string
import y2024/runner.{run as run2024}

pub fn run(input_raw: BitArray, year: Int, day: Int) -> #(String, String) {
  let assert Ok(input_z) = bit_array.to_string(input_raw)
  // drop NUL byte
  let input = string.drop_end(input_z, 1)
  case year {
    2024 -> run2024(input, day)
    _ -> panic as "Invalid year/day"
  }
}