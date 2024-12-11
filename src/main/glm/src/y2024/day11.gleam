import gleam/int
import gleam/list
import gleam/string

pub fn day11(input: String) -> #(String, String) {
  let stones = input |> string.split(" ")
  let s1 = list.fold(stones, 0, fn(acc, stone) { acc + blink(stone, 25) })
  let s2 = list.fold(stones, 0, fn(acc, stone) { acc + blink(stone, 75) })

  #(int.to_string(s1), int.to_string(s2))
}

fn blink(stone: String, times: Int) -> Int {
  let digit_count = string.length(stone)
  case times, stone {
    0, _ -> 1
    _, "0" -> blink("1", times - 1)
    _, _ if digit_count % 2 == 0 -> {
      stone
      |> string.to_graphemes()
      |> list.sized_chunk(digit_count / 2)
      |> list.map(fn(tokens) {
        case list.drop_while(tokens, fn(token) { token == "0" }) {
          [] -> "0"
          tokens -> string.concat(tokens)
        }
      })
      |> list.fold(0, fn(acc, stone) { acc + blink(stone, times - 1) })
    }
    _, _ -> {
      let assert Ok(num) = int.parse(stone)
      blink(int.to_string(num * 2024), times - 1)
    }
  }
}
