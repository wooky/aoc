import gleam/deque.{type Deque}
import gleam/int
import gleam/list
import gleam/order
import gleam/string

pub fn day09(input: String) -> #(String, String) {
  let disk_map =
    input
    |> string.to_utf_codepoints()
    |> list.index_map(fn(codepoint, idx) {
      let value = string.utf_codepoint_to_int(codepoint) - 0x30
      case idx % 2 == 0 {
        True -> File(idx / 2, value)
        False -> Empty(value)
      }
    })
    |> deque.from_list()
  let s1 = checksum1(0, disk_map, 0)
  let s2 = checksum2(0, disk_map, 0)

  #(int.to_string(s1), int.to_string(s2))
}

type Token {
  File(file_id: Int, remaining: Int)
  Empty(empty: Int)
}

fn checksum1(checksum: Int, disk_map: Deque(Token), block_id: Int) -> Int {
  case deque.pop_front(disk_map) {
    Ok(#(File(_, 0), disk_map)) | Ok(#(Empty(0), disk_map)) ->
      checksum1(checksum, disk_map, block_id)
    Ok(#(File(front_file_id, remaining), disk_map)) -> {
      let checksum = checksum + block_id * front_file_id
      let disk_map =
        deque.push_front(disk_map, File(front_file_id, remaining - 1))
      checksum1(checksum, disk_map, block_id + 1)
    }
    Ok(#(Empty(empty) as front_token, disk_map)) -> {
      case deque.pop_back(disk_map) {
        Ok(#(File(back_file_id, remaining) as back_token, disk_map)) -> {
          let disk_map = case int.compare(empty, remaining) {
            order.Lt ->
              disk_map
              |> deque.push_front(File(back_file_id, empty))
              |> deque.push_back(File(back_file_id, remaining - empty))
            order.Eq -> deque.push_front(disk_map, back_token)
            order.Gt ->
              disk_map
              |> deque.push_front(Empty(empty - remaining))
              |> deque.push_front(back_token)
          }
          checksum1(checksum, disk_map, block_id)
        }
        Ok(#(Empty(_), disk_map)) -> {
          let disk_map = deque.push_front(disk_map, front_token)
          checksum1(checksum, disk_map, block_id)
        }
        Error(_) -> checksum
      }
    }
    Error(_) -> checksum
  }
}

fn checksum2(checksum: Int, disk_map: Deque(Token), block_id: Int) -> Int {
  case deque.pop_front(disk_map) {
    Ok(#(File(_, 0), disk_map)) | Ok(#(Empty(0), disk_map)) ->
      checksum2(checksum, disk_map, block_id)
    Ok(#(File(front_file_id, remaining), disk_map)) -> {
      let checksum = checksum + block_id * front_file_id
      let disk_map =
        deque.push_front(disk_map, File(front_file_id, remaining - 1))
      checksum2(checksum, disk_map, block_id + 1)
    }
    Ok(#(Empty(empty), disk_map)) -> {
      case move_file_fully(empty, disk_map, False) {
        Ok(disk_map) -> checksum2(checksum, disk_map, block_id)
        Error(_) -> checksum2(checksum, disk_map, block_id + empty)
      }
    }
    Error(_) -> checksum
  }
}

fn move_file_fully(
  capacity: Int,
  disk_map: Deque(Token),
  push_back_empties: Bool,
) -> Result(Deque(Token), Nil) {
  case deque.pop_back(disk_map) {
    Ok(#(File(_, remaining) as back_token, disk_map)) -> {
      case int.compare(capacity, remaining) {
        order.Lt -> {
          case move_file_fully(capacity, disk_map, True) {
            Ok(disk_map) -> {
              let disk_map = deque.push_back(disk_map, back_token)
              Ok(disk_map)
            }
            Error(_) -> Error(Nil)
          }
        }
        order.Eq -> {
          let disk_map =
            disk_map
            |> deque.push_front(back_token)
            |> deque.push_back(Empty(remaining))
          Ok(disk_map)
        }
        order.Gt -> {
          let disk_map =
            disk_map
            |> deque.push_front(Empty(capacity - remaining))
            |> deque.push_front(back_token)
            |> deque.push_back(Empty(remaining))
          Ok(disk_map)
        }
      }
    }
    Ok(#(Empty(_) as back_token, disk_map)) -> {
      case
        move_file_fully(capacity, disk_map, push_back_empties),
        push_back_empties
      {
        Ok(disk_map), True -> Ok(deque.push_back(disk_map, back_token))
        result, _ -> result
      }
    }
    Error(_) -> Error(Nil)
  }
}
