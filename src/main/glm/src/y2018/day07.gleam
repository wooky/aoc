import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/set.{type Set}
import gleam/string

pub fn day07(input: String) -> #(String, String) {
  let assert Ok(re) =
    regexp.from_string("Step (.) must be finished before step (.) can begin\\.")
  let conditions =
    input
    |> string.split("\n")
    |> list.fold(dict.new(), fn(conditions, line) {
      let assert [
        regexp.Match(content: _, submatches: [Some(before), Some(after)]),
      ] = regexp.scan(re, line)
      conditions
      |> dict.upsert(after, fn(x) {
        case x {
          Some(befores) -> set.insert(befores, before)
          None -> set.from_list([before])
        }
      })
      |> dict.upsert(before, fn(x) {
        case x {
          Some(b) -> b
          None -> set.new()
        }
      })
    })
  let s1 = make_chain([], conditions) |> string.concat()
  let s2 = time_construction(0, [], conditions, dict.new())

  #(s1, int.to_string(s2))
}

const max_workers = 5

fn make_chain(
  processed: List(String),
  unprocessed: Dict(String, Set(String)),
) -> List(String) {
  case next_jobs(processed, unprocessed) {
    [x, ..] ->
      make_chain(list.append(processed, [x]), dict.delete(unprocessed, x))
    [] -> processed
  }
}

fn next_jobs(
  processed: List(String),
  unprocessed: Dict(String, Set(String)),
) -> List(String) {
  unprocessed
  |> dict.filter(fn(_, conditions) {
    conditions |> set.drop(processed) |> set.is_empty()
  })
  |> dict.keys()
  |> list.sort(string.compare)
}

fn time_construction(
  time: Int,
  processed: List(String),
  unprocessed: Dict(String, Set(String)),
  workers: Dict(String, Int),
) -> Int {
  let jobs =
    next_jobs(processed, unprocessed)
    |> list.take(max_workers - dict.size(workers))
  let unprocessed = dict.drop(unprocessed, jobs)

  let job_times = {
    use job <- list.map(jobs)
    let assert [codepoint] = string.to_utf_codepoints(job)
    #(job, time + string.utf_codepoint_to_int(codepoint) - 0x40 + 60)
  }
  let workers = dict.merge(workers, dict.from_list(job_times))

  case dict.is_empty(unprocessed) {
    True -> dict.fold(workers, 0, fn(acc, _, t) { int.max(acc, t) })
    False -> {
      let time =
        dict.fold(workers, 999_999_999, fn(acc, _, t) { int.min(acc, t) })
      let just_processed =
        workers
        |> dict.filter(fn(_, t) { t == time })
        |> dict.keys()
        |> list.sort(string.compare)
      let processed = list.append(processed, just_processed)
      let workers = dict.drop(workers, processed)
      time_construction(time, processed, unprocessed, workers)
    }
  }
}
