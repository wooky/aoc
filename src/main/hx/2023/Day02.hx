import haxe.Exception;
import haxe.display.Position.Range;
import sys.io.File;

class Day02 {
  static function main() {
    final lines = File.getContent("../../../../input/2023/day02.txt").split("\n");
    final games = new Array<Game>();
    final possibleCubes = new CubeSet([new Cube(12, "red"), new Cube(13, "green"), new Cube(14, "blue")]);
    var possibleGames = 0;
    var totalPower = 0;

    for (line in lines) {
      if (line == "") {
        continue;
      }
      final reGame = ~/Game (\d+):/;
      reGame.match(line);
      final game = new Game(Std.parseInt(reGame.matched(1)));

      final reCube = ~/ (\d+) (\w+)([,;]?)/;
      reCube.match(reGame.matchedRight());
      var cubeSet = new CubeSet();
      do {
        cubeSet.push(new Cube(Std.parseInt(reCube.matched(1)), reCube.matched(2)));
        switch (reCube.matched(3)) {
          case ",": {}
          case ";"|"":
            game.cubeSets.push(cubeSet);
            cubeSet = new CubeSet();
          default: throw new Exception("Unexpected cube terminator");
        }
      } while (reCube.match(reCube.matchedRight()));

      possibleGames += game.isGamePossible(possibleCubes);
      totalPower += game.calcPower();
    }

    trace(possibleGames);
    trace(totalPower);
  }
}

class Game {
  public final number:Int;
  public final cubeSets = new Array<CubeSet>();

  public function new(number) {
    this.number = number;
  }

  public function isGamePossible(possibleCubes:CubeSet):Int {
    for (cubeSet in cubeSets) {
      if (cubeSet > possibleCubes) {
        return 0;
      }
    }
    return number;
  }

  public function calcPower():Int {
    final maxCubes = new Map<String, Int>();
    for (cubeSet in cubeSets) {
      for (cube in cubeSet) {
        final curQty = maxCubes.get(cube.color);
        if (curQty == null || curQty < cube.qty) {
          maxCubes.set(cube.color, cube.qty);
        }
      }
    }

    var power = 1;
    for (cube in maxCubes) {
      power *= cube;
    }
    return power;
  }
}

@:forward(push, iterator)
abstract CubeSet(Array<Cube>) {
  public function new(cubes:Array<Cube> = null) {
    this = (cubes != null) ? cubes : [];
  }

  @:op(A > B) public static function gt(lhs:CubeSet, rhs:CubeSet):Bool {
    for (lhsCube in lhs) {
      for (rhsCube in rhs) {
        if (lhsCube.color == rhsCube.color && lhsCube.qty > rhsCube.qty) {
          return true;
        }
      }
    }
    return false;
  }
}

class Cube {
  public final qty:Int;
  public final color:String;

  public function new(qty, color) {
    this.qty = qty;
    this.color = color;
  }
}
