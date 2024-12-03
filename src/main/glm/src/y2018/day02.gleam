import gleam/dict
import gleam/int
import gleam/list.{Continue, Stop}
import gleam/option.{None, Some}
import gleam/string

pub fn day02(input: String) -> #(String, String) {
  let lines = string.split(input, "\n")
  let res1 =
    lines
    |> list.fold(S1(0, 0), fn(acc, line) {
      let tally =
        line
        |> string.to_utf_codepoints()
        |> list.fold(dict.new(), fn(tally, c) {
          dict.upsert(tally, c, fn(x) {
            case x {
              Some(i) -> i + 1
              None -> 1
            }
          })
        })
        |> dict.values()
        |> list.fold(S1(0, 0), fn(tally, x) {
          case tally, x {
            S1(twice: 0, thrice: _), 2 -> S1(..tally, twice: 1)
            S1(twice: _, thrice: 0), 3 -> S1(..tally, thrice: 1)
            _, _ -> tally
          }
        })
      S1(twice: acc.twice + tally.twice, thrice: acc.thrice + tally.thrice)
    })
  let s1 = res1.twice * res1.thrice

  let assert [target, ..boxes] = lines
  let s2 = find_similar(target, boxes)

  #(int.to_string(s1), s2)
}

type S1 {
  S1(twice: Int, thrice: Int)
}

type BoxSearch {
  NoConflict(idx: Int)
  Conflict(conflicting_idx: Int, idx: Int)
  Failure
}

fn find_similar(target: String, boxes: List(String)) -> String {
  let target_chars = string.to_utf_codepoints(target)
  let res =
    list.fold_until(boxes, None, fn(_, box) {
      let box_chars = string.to_utf_codepoints(box)
      let assert Ok(zip) = list.strict_zip(target_chars, box_chars)
      let res =
        list.fold_until(zip, NoConflict(0), fn(acc, pair) {
          case pair.0 == pair.1, acc {
            True, NoConflict(idx) -> Continue(NoConflict(idx: idx + 1))
            True, Conflict(conflicting_idx: _, idx: idx) as conflict ->
              Continue(Conflict(..conflict, idx: idx + 1))
            False, NoConflict(idx) ->
              Continue(Conflict(conflicting_idx: idx, idx: idx + 1))
            False, Conflict(_, _) -> Stop(Failure)
            _, _ -> panic
          }
        })
      case res {
        Conflict(conflicting_idx, _) -> Stop(Some(conflicting_idx))
        Failure -> Continue(None)
        _ -> panic
      }
    })
  case res {
    Some(conflicting_idx) ->
      string.from_utf_codepoints(list.take(target_chars, conflicting_idx))
      <> string.from_utf_codepoints(list.drop(target_chars, conflicting_idx + 1))
    None -> {
      let assert [new_target, ..new_boxes] = boxes
      find_similar(new_target, new_boxes)
    }
  }
}
