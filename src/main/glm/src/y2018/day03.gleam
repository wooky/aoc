import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/set
import gleam/string
import gleam/yielder

pub fn day03(input: String) -> #(String, String) {
  let assert Ok(re) =
    regexp.from_string("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)")
  let bounding_boxes =
    input
    |> string.split("\n")
    |> list.map(fn(line) {
      let assert [match] = regexp.scan(re, line)
      let assert [Ok(id), Ok(x1), Ok(y1), Ok(w), Ok(h)] =
        match.submatches |> option.values() |> list.map(int.parse)
      BoundingBox(id, x1, y1, x1 + w - 1, y1 + h - 1)
    })
  let claims =
    list.fold(bounding_boxes, dict.new(), fn(acc, bb) {
      use acc, y <- yielder.fold(yielder.range(bb.y1, bb.y2), acc)
      use acc, x <- yielder.fold(yielder.range(bb.x1, bb.x2), acc)
      dict.upsert(acc, #(x, y), fn(prev) {
        case prev {
          Some(prev) -> [bb.id, ..prev]
          None -> [bb.id]
        }
      })
    })
    |> dict.values()
  let s1 =
    claims
    |> list.fold(0, fn(acc, claims) {
      acc
      + case claims {
        [_] -> 0
        _ -> 1
      }
    })
  let assert [s2] =
    claims
    |> list.fold(
      set.from_list(list.range(1, list.length(bounding_boxes))),
      fn(acc, claims) {
        case claims {
          [_] -> acc
          _ -> set.drop(acc, claims)
        }
      },
    )
    |> set.to_list()

  #(int.to_string(s1), int.to_string(s2))
}

type BoundingBox {
  BoundingBox(id: Int, x1: Int, y1: Int, x2: Int, y2: Int)
}
