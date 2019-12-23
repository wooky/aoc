package AOC.AOC_2019.Day03 is
   type Day_03 is new Day.Day with null record;

   overriding procedure Init (D : Day_03; Root : String);
   overriding function Part_1 (D : Day_03) return String;
   overriding function Part_2 (D : Day_03) return String;
end AOC.AOC_2019.Day03;
