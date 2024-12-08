import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string

pub fn day02(input: String) -> #(String, String) {
  let lines = input |> string.split("\n") |> list.map(string.to_utf_codepoints)
  let res1 =
    lines
    |> list.fold(S1(0, 0), fn(acc, line) {
      let tally =
        line
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
}

fn find_similar(
  target: List(UtfCodepoint),
  boxes: List(List(UtfCodepoint)),
) -> String {
  let res =
    list.try_fold(boxes, Nil, fn(_, box) {
      let assert Ok(zip) = list.strict_zip(target, box)
      let res =
        list.try_fold(zip, NoConflict(0), fn(acc, pair) {
          case pair.0 == pair.1, acc {
            True, NoConflict(idx) -> Ok(NoConflict(idx: idx + 1))
            True, Conflict(conflicting_idx: _, idx: idx) as conflict ->
              Ok(Conflict(..conflict, idx: idx + 1))
            False, NoConflict(idx) ->
              Ok(Conflict(conflicting_idx: idx, idx: idx + 1))
            False, Conflict(_, _) -> Error(Nil)
          }
        })
      case res {
        Ok(Conflict(conflicting_idx, _)) -> Error(conflicting_idx)
        Error(_) -> Ok(Nil)
        _ -> panic
      }
    })
  case res {
    Error(conflicting_idx) -> {
      let assert #(before, [_, ..after]) = list.split(target, conflicting_idx)
      string.from_utf_codepoints(list.append(before, after))
    }
    Ok(_) -> {
      let assert [new_target, ..new_boxes] = boxes
      find_similar(new_target, new_boxes)
    }
  }
}
