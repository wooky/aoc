with Intcode;

package AOC.AOC_2019.Day07 is
   type Day_07 is new Day.Day with private;

   overriding procedure Init (D : in out Day_07; Root : String);
   overriding function Part_1 (D : Day_07) return String;
   overriding function Part_2 (D : Day_07) return String;
private
   type Day_07 is new Day.Day with record
      Compiler : Intcode.Compilers.Compiler;
   end record;
end AOC.AOC_2019.Day07;
