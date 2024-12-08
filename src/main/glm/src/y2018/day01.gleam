import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub fn day01(input: String) -> #(String, String) {
  let assert Ok(frequencies) =
    input
    |> string.split("\n")
    |> list.map(int.parse)
    |> result.all()

  let #(s1, s2) = find_solution(frequencies, 0, set.new())
  #(int.to_string(s1), int.to_string(s2))
}

fn find_solution(
  frequencies: List(Int),
  original_frequency: Int,
  original_encountered: Set(Int),
) -> #(Int, Int) {
  let dupe_finder =
    list.try_fold(
      frequencies,
      NoDupes(original_frequency, original_encountered),
      fn(acc, x) {
        let new_frequency = acc.frequency + x
        case set.contains(acc.encountered, new_frequency) {
          True -> Error(new_frequency)
          False ->
            Ok(NoDupes(
              new_frequency,
              set.insert(acc.encountered, new_frequency),
            ))
        }
      },
    )

  case dupe_finder {
    Ok(NoDupes(frequency, encountered)) -> #(
      frequency,
      find_solution(frequencies, frequency, encountered).1,
    )
    Error(x) -> #(-666, x)
  }
}

type NoDupes {
  NoDupes(frequency: Int, encountered: Set(Int))
}
