package AOC.AOC_2019.Day05 is
   type Day_05 is new Day.Day with null record;

   overriding procedure Init (D : in out Day_05; Root : String);
   overriding function Part_1 (D : Day_05) return String;
   overriding function Part_2 (D : Day_05) return String;
end AOC.AOC_2019.Day05;
