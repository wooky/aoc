import gleam/int
import gleam/list
import gleam/string
import gleam/yielder

pub fn day05(input: String) -> #(String, String) {
  let chars =
    input |> string.to_utf_codepoints() |> list.map(string.utf_codepoint_to_int)
  let s1 = list.length(react_polymer([], chars, 0))
  let s2 =
    yielder.range(0x41, 0x5a)
    |> yielder.fold(s1, fn(best, purge) {
      int.min(best, list.length(react_polymer([], chars, purge)))
    })

  #(int.to_string(s1), int.to_string(s2))
}

fn react_polymer(
  old_reversed: List(Int),
  chars: List(Int),
  purge: Int,
) -> List(Int) {
  case old_reversed, chars {
    _, [c, ..rest] if c == purge || c == purge + 0x20 ->
      react_polymer(old_reversed, rest, purge)
    [o, ..old_rest], [c, ..rest] if o - c == 0x20 || c - o == 0x20 ->
      react_polymer(old_rest, rest, purge)
    _, [c, ..rest] -> react_polymer([c, ..old_reversed], rest, purge)
    _, [] -> old_reversed
  }
}
