import gleam/int
import gleam/list
import gleam/string

pub fn day09(input: String) -> #(String, String) {
  let assert [first_size, ..disk_map] =
    input
    |> string.to_utf_codepoints()
    |> list.map(fn(x) { string.utf_codepoint_to_int(x) - 0x30 })
  let assert [last_size, ..reverse_disk_map] = list.reverse(disk_map)
  let reverse_file_id = list.length(disk_map) / 2

  let s1 =
    calc_checksum(
      0,
      DiskState(File(first_size), disk_map, 0),
      DiskState(File(last_size), reverse_disk_map, reverse_file_id),
      0,
    )

  #(int.to_string(s1), "TODO")
}

type ReadState {
  File(remaining: Int)
  Empty(remaining: Int)
}

type DiskState {
  DiskState(read_state: ReadState, disk_map: List(Int), file_id: Int)
}

fn calc_checksum(
  checksum: Int,
  front_state: DiskState,
  back_state: DiskState,
  block_id: Int,
) -> Int {
  case front_state, back_state {
    DiskState(File(0), _, _), DiskState(Empty(_), _, _) -> checksum
    DiskState(File(0), front_disk_map, _), _ -> {
      let assert [remaining, ..front_disk_map] = front_disk_map
      calc_checksum(
        checksum,
        DiskState(
          ..front_state,
          read_state: Empty(remaining),
          disk_map: front_disk_map,
        ),
        back_state,
        block_id,
      )
    }
    DiskState(File(remaining), _, front_file_id), _ -> {
      let checksum = checksum + block_id * front_file_id
      calc_checksum(
        checksum,
        DiskState(..front_state, read_state: File(remaining - 1)),
        back_state,
        block_id + 1,
      )
    }
    DiskState(Empty(0), _, front_file_id), DiskState(_, _, back_file_id)
      if front_file_id + 1 == back_file_id
    ->
      calc_checksum(
        checksum,
        back_state,
        DiskState(Empty(-1), [], -1),
        block_id,
      )
    DiskState(Empty(0), front_disk_map, front_file_id), _ -> {
      let assert [remaining, ..front_disk_map] = front_disk_map
      calc_checksum(
        checksum,
        DiskState(File(remaining), front_disk_map, front_file_id + 1),
        back_state,
        block_id,
      )
    }
    DiskState(Empty(_), _, front_file_id), DiskState(File(0), _, back_file_id)
      if back_file_id - 1 == front_file_id
    -> checksum
    _, DiskState(File(0), back_disk_map, back_file_id) -> {
      let assert [_, remaining, ..back_disk_map] = back_disk_map
      calc_checksum(
        checksum,
        front_state,
        DiskState(File(remaining), back_disk_map, back_file_id - 1),
        block_id,
      )
    }
    DiskState(Empty(empty), _, _), DiskState(File(remaining), _, back_file_id) -> {
      let checksum = checksum + block_id * back_file_id
      calc_checksum(
        checksum,
        DiskState(..front_state, read_state: Empty(empty - 1)),
        DiskState(..back_state, read_state: File(remaining - 1)),
        block_id + 1,
      )
    }
    _, _ -> panic
  }
}
