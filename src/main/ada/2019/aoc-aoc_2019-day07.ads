package AOC.AOC_2019.Day07 is
   type Day_07 is new Day.Day with null record;

   overriding procedure Init (D : Day_07; Root : String);
   overriding function Part_1 (D : Day_07) return String;
   overriding function Part_2 (D : Day_07) return String;
end AOC.AOC_2019.Day07;
