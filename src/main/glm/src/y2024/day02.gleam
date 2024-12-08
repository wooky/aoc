import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn day02(input: String) -> #(String, String) {
  let #(s1, s2) =
    input
    |> string.split("\n")
    |> list.fold(#(0, 0), fn(safes, line) {
      let assert Ok(levels) =
        line
        |> string.split(" ")
        |> list.map(int.parse)
        |> result.all()
      let safe1 = test_report(levels)

      let safe2 = {
        use failed_idx <- result.try_recover(safe1)
        use _ <- result.try_recover(
          test_report(list_remove(levels, failed_idx)),
        )
        use failed_idx <- result.try_recover(
          test_report(list_remove(levels, failed_idx + 1)),
        )
        case failed_idx {
          1 -> test_report(list_remove(levels, 0))
          _ -> Error(0)
        }
      }

      #(safes.0 + result.unwrap(safe1, 0), safes.1 + result.unwrap(safe2, 0))
    })

  #(int.to_string(s1), int.to_string(s2))
}

type Order {
  Started(Int)
  Increasing(Int, Int)
  Decreasing(Int, Int)
}

fn test_report(levels: List(Int)) -> Result(Int, Int) {
  let assert [start, ..rest] = levels
  list.try_fold(rest, Started(start), fn(acc, curr) {
    case acc {
      Started(prev) ->
        case is_increasing(prev, curr), is_decreasing(prev, curr) {
          True, False -> Ok(Increasing(curr, 1))
          False, True -> Ok(Decreasing(curr, 1))
          _, _ -> Error(0)
        }
      Increasing(prev, idx) ->
        case is_increasing(prev, curr) {
          True -> Ok(Increasing(curr, idx + 1))
          _ -> Error(idx)
        }
      Decreasing(prev, idx) ->
        case is_decreasing(prev, curr) {
          True -> Ok(Decreasing(curr, idx + 1))
          _ -> Error(idx)
        }
    }
  })
  |> result.map(fn(_) { 1 })
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
  let assert #(before, [_, ..after]) = list.split(l, idx)
  list.append(before, after)
}
