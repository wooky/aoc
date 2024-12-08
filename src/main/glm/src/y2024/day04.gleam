import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string
import lib/coord.{type Coord, type Delta, Coord, Delta}

pub fn day04(input: String) -> #(String, String) {
  let #(chars, _) =
    input
    |> string.to_utf_codepoints()
    |> list.fold(#(dict.new(), Coord(0, 0)), fn(acc, codepoint) {
      let #(chars, coord) = acc
      let char = string.utf_codepoint_to_int(codepoint)
      case char {
        0x0a -> #(chars, coord.crlf(coord))
        _ -> #(dict.insert(chars, coord, char), coord.next_column(coord))
      }
    })
  let s1 = solution1(chars)
  let s2 = solution2(chars)

  #(int.to_string(s1), int.to_string(s2))
}

type Chars =
  Dict(Coord, Int)

fn solution1(chars: Chars) -> Int {
  let mas = [0x4d, 0x41, 0x53]

  dict.fold(chars, 0, fn(matches, coord, c) {
    case c {
      0x58 -> {
        let new_matches =
          coord.neighbor_deltas()
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
  delta: Delta,
  coord: Coord,
  stack: List(Int),
) -> Int {
  case stack {
    [] -> 1
    [c, ..rest] -> {
      let coord = coord.add(coord, delta)
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
      let delta = Delta(drow, dcol)
      case
        dict.get(chars, coord.add(coord, delta)),
        dict.get(chars, coord.sub(coord, delta))
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
