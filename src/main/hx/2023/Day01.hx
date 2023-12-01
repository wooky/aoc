import haxe.Exception;
import sys.io.File;
using StringTools;

class Day01 {
  static function main() {
    final lines = File.getContent("../../../../input/2023/day01.txt").split("\n");

    var s1 = 0;
    var s2 = 0;

    for (line in lines) {
      if (line == "") {
        continue;
      }

      s1 += getCalibration(~/[0-9]/, line, getDigit);
      s2 += getCalibration(~/[0-9]|one|two|three|four|five|six|seven|eight|nine/, line, getWord);
    }

    trace(s1);
    trace(s2);
  }

  static function getCalibration(pattern:EReg, line:String, digitGetter:String->Int):Int {
    pattern.match(line);
    var lastMatch = pattern.matched(0);
    var calibration = digitGetter(lastMatch) * 10;
    while (pattern.matchSub(line, pattern.matchedPos().pos + 1)) {
      lastMatch = pattern.matched(0);
    }
    calibration += digitGetter(lastMatch);
    return calibration;
  }

  static function getDigit(s:String):Null<Int> {
    if (s.length != 1) {
      return null;
    }
    return s.charCodeAt(0) - '0'.code;
  }

  static function getWord(s:String):Int {
    final singleDigit = getDigit(s);
    if (singleDigit != null) {
      return singleDigit;
    }
    switch (s) {
      case "one": return 1;
      case "two": return 2;
      case "three": return 3;
      case "four": return 4;
      case "five": return 5;
      case "six": return 6;
      case "seven": return 7;
      case "eight": return 8;
      case "nine": return 9;
      default: throw new Exception("Invalid digit " + s);
    }
  }
}
