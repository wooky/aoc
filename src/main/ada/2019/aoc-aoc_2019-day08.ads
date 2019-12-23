package AOC.AOC_2019.Day08 is
   type Day_08 is new Day.Day with null record;

   overriding procedure Init (D : Day_08; Root : String);
   overriding function Part_1 (D : Day_08) return String;
   overriding function Part_2 (D : Day_08) return String;
end AOC.AOC_2019.Day08;
