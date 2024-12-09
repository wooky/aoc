import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleam/yielder

pub fn day08(input: String) -> #(String, String) {
  let assert Ok(tokens) =
    input |> string.split(" ") |> list.map(int.parse) |> result.all()
  let assert Node([], s1, s2) = read_node(tokens)

  #(int.to_string(s1), int.to_string(s2))
}

type Tokens =
  List(Int)

type Node {
  Node(tokens: Tokens, s1: Int, s2: Int)
}

fn read_node(tokens: Tokens) -> Node {
  let assert [child_count, metadata_count, ..tokens] = tokens
  case child_count {
    0 -> {
      let #(metadata, tokens) = list.split(tokens, metadata_count)
      let metadata_sum = int.sum(metadata)
      Node(tokens, metadata_sum, metadata_sum)
    }
    _ -> {
      let #(children, tokens) =
        yielder.range(1, child_count)
        |> yielder.fold(#(dict.new(), tokens), fn(acc, idx) {
          let #(children, tokens) = acc
          let node = read_node(tokens)
          #(dict.insert(children, idx, node), node.tokens)
        })
      let #(metadata, tokens) = list.split(tokens, metadata_count)
      let s1 =
        dict.fold(children, 0, fn(acc, _, x) { acc + x.s1 }) + int.sum(metadata)
      let s2 =
        list.fold(metadata, 0, fn(acc, x) {
          acc
          + case dict.get(children, x) {
            Ok(v) -> v.s2
            Error(_) -> 0
          }
        })
      Node(tokens, s1, s2)
    }
  }
}
