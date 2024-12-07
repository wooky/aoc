import gleam/int
import gleam/list
import gleam/set.{type Set}
import gleam/string

type Rules =
  Set(#(String, String))

pub fn day05(input: String) -> #(String, String) {
  let assert Update(_, s1, s2) =
    input
    |> string.split("\n")
    |> list.fold(PageOrdering(set.new()), process_line)
  #(int.to_string(s1), int.to_string(s2))
}

type StateMachine {
  PageOrdering(rules: Rules)
  Update(rules: Rules, s1: Int, s2: Int)
}

type ReorganizePages {
  ReorganizePages(middle_page: String, was_reorganized: Bool)
}

fn process_line(sm: StateMachine, line: String) -> StateMachine {
  case sm, line {
    PageOrdering(rules), "" -> Update(rules, 0, 0)
    PageOrdering(rules), _ -> {
      let assert [before, after] = string.split(line, "|")
      PageOrdering(set.insert(rules, #(before, after)))
    }
    Update(rules, s1, s2) as sm, _ -> {
      let update = string.split(line, ",")
      let middle_idx = list.length(update) / 2
      let res =
        reorganize_pages(
          ReorganizePages("", False),
          rules,
          middle_idx,
          update,
          0,
        )
      let assert Ok(middle_page) = int.parse(res.middle_page)
      case res.was_reorganized {
        True -> Update(..sm, s2: s2 + middle_page)
        False -> Update(..sm, s1: s1 + middle_page)
      }
    }
  }
}

fn reorganize_pages(
  state: ReorganizePages,
  rules: Rules,
  middle_idx: Int,
  update: List(String),
  idx: Int,
) -> ReorganizePages {
  case update {
    [] -> state
    [after, ..befores] -> {
      case is_update_organized(after, [], befores, rules) {
        Ok(_) -> {
          let middle_page = case idx == middle_idx {
            True -> after
            False -> state.middle_page
          }
          let state = ReorganizePages(..state, middle_page: middle_page)
          reorganize_pages(state, rules, middle_idx, befores, idx + 1)
        }
        Error(#(befores_okay, before_bad, befores_rest)) -> {
          let update =
            [before_bad]
            |> list.append(befores_okay)
            |> list.append([after])
            |> list.append(befores_rest)
          let state = ReorganizePages(..state, was_reorganized: True)
          reorganize_pages(state, rules, middle_idx, update, idx)
        }
      }
    }
  }
}

fn is_update_organized(
  after: String,
  befores_okay: List(String),
  befores_rest: List(String),
  rules: Rules,
) -> Result(Nil, #(List(String), String, List(String))) {
  case befores_rest {
    [] -> Ok(Nil)
    [before, ..rest] -> {
      case set.contains(rules, #(before, after)) {
        True -> Error(#(befores_okay, before, rest))
        False ->
          is_update_organized(
            after,
            list.append(befores_okay, [before]),
            rest,
            rules,
          )
      }
    }
  }
}
