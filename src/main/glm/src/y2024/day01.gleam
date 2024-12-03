import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

pub fn day01(input: String) -> #(String, String) {
  let #(left_unsorted, right_unsorted, right_counts) =
    input
    |> string.split("\n")
    |> list.fold(#(list.new(), list.new(), dict.new()), fn(acc, line) {
      let assert Ok(#(left_str, right_str)) = string.split_once(line, "   ")
      let assert Ok(left) = int.parse(left_str)
      let assert Ok(right) = int.parse(right_str)
      let increment = fn(x) {
        case x {
          Some(i) -> i + 1
          None -> 1
        }
      }
      #([left, ..acc.0], [right, ..acc.1], dict.upsert(acc.2, right, increment))
    })
  let left = list.sort(left_unsorted, int.compare)
  let right = list.sort(right_unsorted, int.compare)
  let assert Ok(combined) = list.strict_zip(left, right)
  let s1 =
    list.fold(combined, 0, fn(acc, x) { acc + int.absolute_value(x.0 - x.1) })
  let s2 =
    list.fold(left_unsorted, 0, fn(acc, x) {
      acc + x * { right_counts |> dict.get(x) |> result.unwrap(0) }
    })

  #(int.to_string(s1), int.to_string(s2))
}
