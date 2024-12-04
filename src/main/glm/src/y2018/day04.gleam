import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order.{type Order}
import gleam/regexp
import gleam/string
import gleam/yielder
import tempo/date
import tempo/naive_datetime
import tempo/time

pub fn day04(input: String) -> #(String, String) {
  let assert Ok(re) =
    regexp.from_string("\\[(.*?)\\] (?:Guard #(\\d+)|(f)|(w))")
  let shifts_per_guard =
    input
    |> string.split("\n")
    |> list.fold(dict.new(), fn(acc, line) {
      let assert [
        regexp.Match(content: _, submatches: [Some(timestamp_str), ..tokens]),
      ] = regexp.scan(re, line)

      let assert Ok(timestamp) =
        naive_datetime.parse(timestamp_str, "YYYY-MM-DD HH:mm")
      let log_time = timestamp |> naive_datetime.get_time()
      let log_date = case log_time |> time.get_hour() {
        23 -> timestamp |> naive_datetime.get_date() |> date.add(1)
        _ -> timestamp |> naive_datetime.get_date()
      }

      let shift_event = case tokens {
        [Some(id_str)] -> {
          let assert Ok(id) = int.parse(id_str)
          Guard(id)
        }
        [None, Some(_)] -> Sleep(time.get_minute(log_time))
        [None, None, Some(_)] -> Wake(time.get_minute(log_time))
        _ -> panic
      }

      dict.upsert(acc, date.to_tuple(log_date), fn(x) {
        case x {
          Some(rest) -> [shift_event, ..rest]
          None -> [shift_event]
        }
      })
    })
    |> dict.fold(dict.new(), fn(acc, _, unsorted_shift) {
      let shift = list.sort(unsorted_shift, normalize_shift)
      let assert [Guard(guard_id), ..events] = shift
      dict.upsert(acc, guard_id, fn(x) {
        case x {
          Some(i) -> list.flatten([events, i])
          None -> events
        }
      })
    })
  let sleepiest_guard_id = key_for_max_value(shifts_per_guard, get_sleep_time)
  let assert Ok(sleepiest_guard) =
    dict.get(shifts_per_guard, sleepiest_guard_id)
  let sleepiest_minute =
    key_for_max_value(
      get_sleep_minute_count(dict.new(), sleepiest_guard),
      function.identity,
    )
  let s1 = sleepiest_guard_id * sleepiest_minute

  #(int.to_string(s1), "TODO")
}

type ShiftEvent {
  Guard(Int)
  Sleep(Int)
  Wake(Int)
}

fn normalize_shift(a: ShiftEvent, b: ShiftEvent) -> Order {
  case a {
    Guard(_) -> order.Lt
    Sleep(a_idx) | Wake(a_idx) ->
      case b {
        Guard(_) -> order.Gt
        Sleep(b_idx) | Wake(b_idx) -> int.compare(a_idx, b_idx)
      }
  }
}

fn get_sleep_time(events: List(ShiftEvent)) -> Int {
  case events {
    [] -> 0
    [Sleep(sleep), Wake(wake), ..rest] -> wake - sleep + get_sleep_time(rest)
    _ -> panic
  }
}

fn get_sleep_minute_count(
  acc: Dict(Int, Int),
  events: List(ShiftEvent),
) -> Dict(Int, Int) {
  case events {
    [] -> acc
    [Sleep(sleep), Wake(wake), ..rest] -> {
      let acc =
        yielder.range(sleep, wake - 1)
        |> yielder.fold(acc, fn(acc, t) {
          dict.upsert(acc, t, fn(x) {
            case x {
              Some(i) -> i + 1
              None -> 1
            }
          })
        })
      get_sleep_minute_count(acc, rest)
    }
    _ -> panic
  }
}

fn key_for_max_value(d: Dict(Int, a), max_by: fn(a) -> Int) -> Int {
  dict.fold(d, #(-1, -1), fn(best, k, v) {
    let potential_max = max_by(v)
    case potential_max > best.1 {
      True -> #(k, potential_max)
      False -> best
    }
  }).0
}
