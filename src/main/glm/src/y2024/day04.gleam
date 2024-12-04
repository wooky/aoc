import gleam/int
import gleam/list
import gleam/string

pub fn day04(input: String) -> #(String, String) {
  let chars =
    input |> string.to_utf_codepoints() |> list.map(string.utf_codepoint_to_int)
  let size =
    chars
    |> list.fold_until(0, fn(idx, c) {
      case c {
        0x0a -> list.Stop(idx)
        _ -> list.Continue(idx + 1)
      }
    })
  let s1 = solution1(chars, size)
  let s2 = solution2(chars, size)

  #(int.to_string(s1), int.to_string(s2))
}

type Matches {
  Matches(matches: Int, row: Int, col: Int)
}

fn solution1(chars: List(Int), size: Int) -> Int {
  let deltas = [
    #(-1, -1),
    #(-1, 0),
    #(-1, 1),
    #(0, -1),
    #(0, 1),
    #(1, -1),
    #(1, 0),
    #(1, 1),
  ]
  let mas = [0x4d, 0x41, 0x53]

  list.fold(chars, Matches(0, 0, 0), fn(matches, c) {
    case c {
      0x58 -> {
        let new_matches =
          deltas
          |> list.map(fn(delta) {
            check_match(chars, size, delta, matches.row, matches.col, mas)
          })
          |> int.sum()
        Matches(
          ..matches,
          matches: matches.matches + new_matches,
          col: matches.col + 1,
        )
      }
      0x0a -> Matches(..matches, row: matches.row + 1, col: 0)
      _ -> Matches(..matches, col: matches.col + 1)
    }
  }).matches
}

fn check_match(
  chars: List(Int),
  size: Int,
  delta: #(Int, Int),
  row: Int,
  col: Int,
  stack: List(Int),
) -> Int {
  case stack {
    [] -> 1
    [c, ..rest] -> {
      let row = row + delta.0
      let col = col + delta.1
      case does_char_match(c, chars, size, row, col) {
        True -> check_match(chars, size, delta, row, col, rest)
        False -> 0
      }
    }
  }
}

fn does_char_match(
  needle: Int,
  chars: List(Int),
  size: Int,
  row: Int,
  col: Int,
) -> Bool {
  case row, col {
    row, col if row < 0 || row >= size || col < 0 || col >= size -> False
    row, col -> {
      let idx = { size + 1 } * row + col
      let assert [c, ..] = list.drop(chars, idx)
      needle == c
    }
  }
}

fn solution2(chars: List(Int), size: Int) -> Int {
  list.fold(chars, Matches(0, 0, 0), fn(matches, c) {
    let diag = fn(drow, dcol) {
      does_char_match(0x4d, chars, size, matches.row + drow, matches.col + dcol)
      && does_char_match(
        0x53,
        chars,
        size,
        matches.row - drow,
        matches.col - dcol,
      )
    }
    case c {
      0x41 -> {
        let new_matches = case
          { diag(-1, -1) || diag(1, 1) } && { diag(-1, 1) || diag(1, -1) }
        {
          True -> 1
          False -> 0
        }
        Matches(
          ..matches,
          matches: matches.matches + new_matches,
          col: matches.col + 1,
        )
      }
      0x0a -> Matches(..matches, row: matches.row + 1, col: 0)
      _ -> Matches(..matches, col: matches.col + 1)
    }
  }).matches
}
