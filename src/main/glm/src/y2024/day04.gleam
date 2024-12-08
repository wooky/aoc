import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string

pub fn day04(input: String) -> #(String, String) {
  let #(chars, _) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(#(dict.new(), #(0, 0)), fn(acc, codepoint) {
      let #(chars, #(row, col) as coord) = acc
      let char = string.utf_codepoint_to_int(codepoint)
      case char {
        0x0a -> #(chars, #(row + 1, 0))
        _ -> #(dict.insert(chars, coord, char), #(row, col + 1))
      }
    })
  let s1 = solution1(chars)
  let s2 = solution2(chars)

  #(int.to_string(s1), int.to_string(s2))
}

type Coord =
  #(Int, Int)

type Chars =
  Dict(Coord, Int)

fn solution1(chars: Chars) -> Int {
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

  dict.fold(chars, 0, fn(matches, coord, c) {
    case c {
      0x58 -> {
        let new_matches =
          deltas
          |> list.map(fn(delta) { check_match(chars, delta, coord, mas) })
          |> int.sum()
        matches + new_matches
      }
      _ -> matches
    }
  })
}

fn check_match(
  chars: Chars,
  delta: Coord,
  coord: Coord,
  stack: List(Int),
) -> Int {
  case stack {
    [] -> 1
    [c, ..rest] -> {
      let coord = #(coord.0 + delta.0, coord.1 + delta.1)
      case dict.get(chars, coord) {
        Ok(match) if match == c -> check_match(chars, delta, coord, rest)
        _ -> 0
      }
    }
  }
}

fn solution2(chars: Chars) -> Int {
  dict.fold(chars, 0, fn(matches, coord, c) {
    let diag = fn(drow, dcol) {
      case
        dict.get(chars, #(coord.0 + drow, coord.1 + dcol)),
        dict.get(chars, #(coord.0 - drow, coord.1 - dcol))
      {
        Ok(0x4d), Ok(0x53) -> True
        _, _ -> False
      }
    }
    case c {
      0x41 -> {
        let new_matches = case
          { diag(-1, -1) || diag(1, 1) } && { diag(-1, 1) || diag(1, -1) }
        {
          True -> 1
          False -> 0
        }
        matches + new_matches
      }
      _ -> matches
    }
  })
}
