package AOC.AOC_2019.Day01 is
   type Day_01 is new Day.Day with null record;

   overriding procedure Init (D : Day_01; Root : String);
   overriding function Part_1 (D : Day_01) return String;
   overriding function Part_2 (D : Day_01) return String;
end AOC.AOC_2019.Day01;
