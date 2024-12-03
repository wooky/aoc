import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp

pub fn day03(input: String) -> #(String, String) {
  let assert Ok(re) =
    regexp.from_string("mul\\((\\d+),(\\d+)\\)|do(n't)?\\(\\)")
  let res =
    re
    |> regexp.scan(input)
    |> list.fold(Acc(0, 0, True), fn(acc, match) {
      case match.submatches {
        [Some(a_str), Some(b_str)] -> {
          let assert Ok(a) = int.parse(a_str)
          let assert Ok(b) = int.parse(b_str)
          let mul = a * b
          let new_s1 = acc.s1 + mul
          let new_s2 =
            acc.s2
            + case acc.do {
              True -> mul
              False -> 0
            }
          Acc(..acc, s1: new_s1, s2: new_s2)
        }
        [] -> Acc(..acc, do: True)
        [None, None, Some(_)] -> Acc(..acc, do: False)
        _ -> panic
      }
    })

  #(int.to_string(res.s1), int.to_string(res.s2))
}

type Acc {
  Acc(s1: Int, s2: Int, do: Bool)
}
