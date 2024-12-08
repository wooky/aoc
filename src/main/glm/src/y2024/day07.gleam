import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string

pub fn day07(input: String) -> #(String, String) {
  let assert Ok(re) = regexp.from_string("\\d+")
  let #(s1, s2) =
    input
    |> string.split("\n")
    |> list.fold(#(0, 0), fn(acc, line) {
      let assert Ok([expected, current, ..remaining]) =
        regexp.scan(re, line)
        |> list.map(fn(x) { int.parse(x.content) })
        |> result.all()
      case can_calibrate(expected, remaining, current, False) {
        Pure -> #(acc.0 + expected, acc.1 + expected)
        WithConcatenation -> #(acc.0, acc.1 + expected)
        Failure -> acc
      }
    })

  #(int.to_string(s1), int.to_string(s2))
}

type CalibrationResult {
  Pure
  WithConcatenation
  Failure
}

fn can_calibrate(
  expected: Int,
  remaining: List(Int),
  current: Int,
  uses_concatenation: Bool,
) -> CalibrationResult {
  case current > expected, remaining {
    True, _ -> Failure
    False, [] -> {
      case current == expected, uses_concatenation {
        True, True -> WithConcatenation
        True, False -> Pure
        False, _ -> Failure
      }
    }
    False, [x, ..rest] -> {
      use <- or_else(can_calibrate(
        expected,
        rest,
        current + x,
        uses_concatenation,
      ))
      use <- or_else(can_calibrate(
        expected,
        rest,
        current * x,
        uses_concatenation,
      ))
      let assert Ok(next) =
        int.parse(int.to_string(current) <> int.to_string(x))
      can_calibrate(expected, rest, next, True)
    }
  }
}

fn or_else(
  result: CalibrationResult,
  next: fn() -> CalibrationResult,
) -> CalibrationResult {
  case result {
    Pure -> Pure
    WithConcatenation ->
      case next() {
        Pure -> Pure
        _ -> WithConcatenation
      }
    Failure -> next()
  }
}
