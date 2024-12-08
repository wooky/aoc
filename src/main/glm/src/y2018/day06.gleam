import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string
import gleam/yielder
import lib/coord.{type Coord, Coord}

pub fn day06(input: String) -> #(String, String) {
  let assert #(dijkstra, BoundingBox(from, to), _) =
    input
    |> string.split("\n")
    |> list.fold(
      #(Dijkstra(dict.new(), set.new(), dict.new(), set.new()), NoBox, 0),
      fn(acc, line) {
        let #(dijkstra, bounding_box, id) = acc
        let assert [Ok(col), Ok(row)] =
          line |> string.split(", ") |> list.map(int.parse)
        let coord = Coord(row, col)
        let dijkstra = want_claim(dijkstra, id, coord)
        let bounding_box = case bounding_box {
          BoundingBox(from, to) ->
            BoundingBox(
              Coord(int.min(from.row, row), int.min(from.col, col)),
              Coord(int.max(to.row, row), int.max(to.col, col)),
            )
          NoBox -> BoundingBox(coord, coord)
        }
        #(dijkstra, bounding_box, id + 1)
      },
    )
  let dijkstra = process_claims(dijkstra, from, to)
  let s1 =
    dict.fold(dijkstra.claim_count, 0, fn(best, id, count) {
      case set.contains(dijkstra.out_of_bounds, id) {
        True -> best
        False -> int.max(best, count)
      }
    })

  #(int.to_string(s1), "TODO")
}

type BoundingBox {
  BoundingBox(from: Coord, to: Coord)
  NoBox
}

type Dijkstra {
  Dijkstra(
    wanted: Dict(Coord, Set(Int)),
    claimed: Set(Coord),
    claim_count: Dict(Int, Int),
    out_of_bounds: Set(Int),
  )
}

fn want_claim(dijkstra: Dijkstra, id: Int, coord: Coord) -> Dijkstra {
  Dijkstra(
    ..dijkstra,
    wanted: dict.upsert(dijkstra.wanted, coord, fn(x) {
      case x {
        Some(others) -> set.insert(others, id)
        None -> set.from_list([id])
      }
    }),
  )
}

fn process_claims(dijkstra: Dijkstra, from: Coord, to: Coord) -> Dijkstra {
  let wanted = dijkstra.wanted
  let dijkstra = Dijkstra(..dijkstra, wanted: dict.new())
  let dijkstra =
    dict.fold(wanted, dijkstra, fn(dijkstra, coord, ids) {
      let new_claimed = set.insert(dijkstra.claimed, coord)
      let does_claim_exist = set.contains(dijkstra.claimed, coord)
      let is_claim_out_of_bounds =
        coord.row < from.row
        || coord.col < from.col
        || coord.row > to.row
        || coord.col > to.col
      case does_claim_exist, set.to_list(ids), is_claim_out_of_bounds {
        False, [id], True ->
          Dijkstra(
            ..dijkstra,
            out_of_bounds: set.insert(dijkstra.out_of_bounds, id),
          )
        False, [id], False -> {
          let dijkstra =
            coord
            |> coord.manhattan()
            |> yielder.fold(dijkstra, fn(dijkstra, coord) {
              want_claim(dijkstra, id, coord)
            })
          let claim_count =
            dict.upsert(dijkstra.claim_count, id, fn(x) {
              case x {
                Some(count) -> count + 1
                None -> 1
              }
            })
          Dijkstra(..dijkstra, claimed: new_claimed, claim_count: claim_count)
        }
        _, _, _ -> Dijkstra(..dijkstra, claimed: new_claimed)
      }
    })
  case dict.size(dijkstra.wanted) {
    0 -> dijkstra
    _ -> process_claims(dijkstra, from, to)
  }
}
