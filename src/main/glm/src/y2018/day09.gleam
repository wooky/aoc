import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/string

pub fn day09(input: String) -> #(String, String) {
  let assert Ok(re) =
    regexp.from_string("(\\d+) players; last marble is worth (\\d+) points")
  let assert [
    regexp.Match(content: _, submatches: [Some(players), Some(marbles)]),
  ] = regexp.scan(re, input)
  let assert Ok(players) = int.parse(players)
  let assert Ok(marbles) = int.parse(marbles)

  let game = play(Game(dict.new(), [0], 0), 0, 1, players, marbles)
  let s1 = dict.fold(game.scores, 0, fn(acc, _, x) { int.max(acc, x) })

  #(int.to_string(s1), "TODO")
}

type Game {
  Game(scores: Dict(Int, Int), board: List(Int), current_marble_idx: Int)
}

fn play(
  game: Game,
  player: Int,
  marble: Int,
  players: Int,
  marbles: Int,
) -> Game {
  // io.debug(game)
  case marble % 1000 == 0 {
    True -> io.debug(marble)
    False -> marble
  }
  let game = case marble {
    m if m % 23 == 0 -> do_special(game, player, marble)
    _ -> do_normal(game, marble)
  }
  case marble == marbles {
    True -> game
    False -> {
      let player = { player + 1 } % players
      play(game, player, marble + 1, players, marbles)
    }
  }
}

fn do_special(game: Game, player: Int, marble: Int) -> Game {
  let marble_idx = { game.current_marble_idx - 7 } % list.length(game.board)
  // let assert #(before, [removed, ..after]) = case marble_idx {
  //   0 -> #([], game.board)
  //   _ -> list.split(game.board, marble_idx - 1)
  // }
  let assert #(before, [removed, ..after]) = list.split(game.board, marble_idx)
  let scores =
    dict.upsert(game.scores, player, fn(x) {
      case x {
        Some(score) -> score + marble + removed
        None -> marble + removed
      }
    })
  Game(scores, list.append(before, after), marble_idx)
}

fn do_normal(game: Game, marble: Int) -> Game {
  let marble_idx = { game.current_marble_idx + 2 } % list.length(game.board)
  let #(before, after) = list.split(game.board, marble_idx)
  Game(
    ..game,
    board: list.flatten([before, [marble], after]),
    current_marble_idx: marble_idx,
  )
}
