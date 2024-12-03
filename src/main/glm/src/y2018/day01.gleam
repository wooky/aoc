import gleam/int
import gleam/list
import gleam/set.{type Set}
import gleam/string

pub fn day01(input: String) -> #(String, String) {
  let frequencies =
    input
    |> string.split("\n")
    |> list.map(fn(line) {
      let assert Ok(x) = int.parse(line)
      x
    })

  let #(s1, s2) = find_solution(frequencies, 0, set.new())
  #(int.to_string(s1), int.to_string(s2))
}

fn find_solution(
  frequencies: List(Int),
  original_frequency: Int,
  original_encountered: Set(Int),
) -> #(Int, Int) {
  let dupe_finder =
    list.fold_until(
      frequencies,
      NoDupes(original_frequency, original_encountered),
      fn(acc, x) {
        let assert NoDupes(frequency, encountered) = acc
        let new_frequency = frequency + x
        case set.contains(encountered, new_frequency) {
          True -> list.Stop(FoundDupe(new_frequency))
          False ->
            list.Continue(NoDupes(
              new_frequency,
              set.insert(encountered, new_frequency),
            ))
        }
      },
    )

  case dupe_finder {
    NoDupes(frequency, encountered) -> #(
      frequency,
      find_solution(frequencies, frequency, encountered).1,
    )
    FoundDupe(x) -> #(-666, x)
  }
}

type DupeFinder {
  NoDupes(frequency: Int, encountered: Set(Int))
  FoundDupe(Int)
}
