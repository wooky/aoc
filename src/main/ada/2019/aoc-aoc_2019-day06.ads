package AOC.AOC_2019.Day06 is
   type Day_06 is new Day.Day with null record;

   overriding procedure Init (D : in out Day_06; Root : String);
   overriding function Part_1 (D : Day_06) return String;
   overriding function Part_2 (D : Day_06) return String;
end AOC.AOC_2019.Day06;
