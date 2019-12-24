package AOC.AOC_2019.Day02 is
   type Day_02 is new Day.Day with null record;

   overriding procedure Init (D : in out Day_02; Root : String);
   overriding function Part_1 (D : Day_02) return String;
   overriding function Part_2 (D : Day_02) return String;
end AOC.AOC_2019.Day02;
