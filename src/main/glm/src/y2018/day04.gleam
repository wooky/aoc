import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/regexp
import gleam/string
import gleam/yielder
import tempo/date
import tempo/naive_datetime
import tempo/time

pub fn day04(input: String) -> #(String, String) {
  let sleep_stats =
    input
    |> string.split("\n")
    |> collect_shift_events_per_date()
    |> collect_shift_events_per_guard()
    |> collect_sleep_stats()

  let s1 = compute_solution(sleep_stats, fn(ss) { ss.total_sleep })
  let s2 = compute_solution(sleep_stats, fn(ss) { ss.minute_most_slept })

  #(s1, s2)
}

type ShiftEvent {
  Guard(Int)
  Sleep(Int)
  Wake(Int)
}

type SleepStats {
  SleepStats(guard_id: Int, total_sleep: Int, minute_most_slept: Int)
}

type IntermediateSleepStats {
  IntermediateSleepStats(
    total_sleep: Int,
    minute_most_slept: Int,
    minute_most_slept_count: Int,
  )
}

fn collect_shift_events_per_date(
  lines: List(String),
) -> Dict(#(Int, Int, Int), List(ShiftEvent)) {
  let assert Ok(re) =
    regexp.from_string("\\[(.*?)\\] (?:Guard #(\\d+)|(f)|(w))")
  list.fold(lines, dict.new(), fn(acc, line) {
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
}

fn collect_shift_events_per_guard(
  shift_events_per_date: Dict(#(Int, Int, Int), List(ShiftEvent)),
) -> Dict(Int, List(ShiftEvent)) {
  let normalize_shift = fn(a: ShiftEvent, b: ShiftEvent) {
    case a {
      Guard(_) -> order.Lt
      Sleep(a_idx) | Wake(a_idx) ->
        case b {
          Guard(_) -> order.Gt
          Sleep(b_idx) | Wake(b_idx) -> int.compare(a_idx, b_idx)
        }
    }
  }

  dict.fold(shift_events_per_date, dict.new(), fn(acc, _, unsorted_shift) {
    let shift = list.sort(unsorted_shift, normalize_shift)
    let assert [Guard(guard_id), ..events] = shift
    dict.upsert(acc, guard_id, fn(x) {
      case x {
        Some(i) -> list.flatten([events, i])
        None -> events
      }
    })
  })
}

fn collect_sleep_stats(
  shift_events_per_guard: Dict(Int, List(ShiftEvent)),
) -> List(SleepStats) {
  dict.fold(shift_events_per_guard, [], fn(acc, guard_id, events) {
    let res =
      collect_per_minute_sleep_stats(dict.new(), events)
      |> dict.fold(IntermediateSleepStats(0, -1, -1), fn(ims, minute, count) {
        case ims {
          IntermediateSleepStats(total_sleep, _, minute_most_slept_count)
            if count > minute_most_slept_count
          -> IntermediateSleepStats(total_sleep + count, minute, count)
          IntermediateSleepStats(
            total_sleep,
            minute_most_slept,
            minute_most_slept_count,
          ) ->
            IntermediateSleepStats(
              total_sleep + count,
              minute_most_slept,
              minute_most_slept_count,
            )
        }
      })
    [SleepStats(guard_id, res.total_sleep, res.minute_most_slept), ..acc]
  })
}

fn collect_per_minute_sleep_stats(
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
      collect_per_minute_sleep_stats(acc, rest)
    }
    _ -> panic
  }
}

fn compute_solution(
  sleep_stats: List(SleepStats),
  max_by: fn(SleepStats) -> Int,
) -> String {
  let assert Ok(res) =
    list.reduce(sleep_stats, fn(best, next) {
      let current_max = max_by(best)
      let potential_max = max_by(next)
      case potential_max > current_max {
        True -> next
        False -> best
      }
    })
  int.to_string(res.guard_id * res.minute_most_slept)
}
