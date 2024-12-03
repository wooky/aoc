import gleam/int
import gleam/list
import gleam/option
import gleam/string

pub fn day02(input: String) -> #(String, String) {
  let #(s1, s2) =
    input
    |> string.split("\n")
    |> list.fold(#(0, 0), fn(safes, line) {
      let levels =
        line
        |> string.split(" ")
        |> list.map(fn(token) {
          let assert Ok(level) = int.parse(token)
          level
        })
      case test_report(levels) {
        option.None -> #(safes.0 + 1, safes.1 + 1)
        option.Some(failed_idx) -> {
          let still_failing = {
            use _ <- option.then(test_report(list_remove(levels, failed_idx)))
            use _ <- option.then(
              test_report(list_remove(levels, failed_idx + 1)),
            )
            use _ <- option.map(case failed_idx {
              1 -> test_report(list_remove(levels, 0))
              _ -> option.Some(0)
            })
            0
          }
          case still_failing {
            option.None -> #(safes.0, safes.1 + 1)
            option.Some(_) -> safes
          }
        }
      }
    })

  #(int.to_string(s1), int.to_string(s2))
}

type Order {
  Started(Int)
  Increasing(Int, Int)
  Decreasing(Int, Int)
  Unsafe(Int)
}

fn test_report(levels: List(Int)) -> option.Option(Int) {
  let assert [start, ..rest] = levels
  let result =
    list.fold_until(rest, Started(start), fn(acc, curr) {
      case acc {
        Started(prev) ->
          case is_increasing(prev, curr), is_decreasing(prev, curr) {
            True, False -> list.Continue(Increasing(curr, 1))
            False, True -> list.Continue(Decreasing(curr, 1))
            _, _ -> list.Stop(Unsafe(0))
          }
        Increasing(prev, idx) ->
          case is_increasing(prev, curr) {
            True -> list.Continue(Increasing(curr, idx + 1))
            _ -> list.Stop(Unsafe(idx))
          }
        Decreasing(prev, idx) ->
          case is_decreasing(prev, curr) {
            True -> list.Continue(Decreasing(curr, idx + 1))
            _ -> list.Stop(Unsafe(idx))
          }
        _ -> panic
      }
    })
  case result {
    Unsafe(x) -> option.Some(x)
    _ -> option.None
  }
}

fn is_increasing(prev: Int, curr: Int) -> Bool {
  case curr - prev {
    1 | 2 | 3 -> True
    _ -> False
  }
}

fn is_decreasing(prev: Int, curr: Int) -> Bool {
  case curr - prev {
    -1 | -2 | -3 -> True
    _ -> False
  }
}

fn list_remove(l: List(Int), idx: Int) -> List(Int) {
  list.flatten([list.take(l, idx), list.drop(l, idx + 1)])
}
