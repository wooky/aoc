package AOC.AOC_2019 is
   type Runner_2019 is new Runner.Runner with null record;

   overriding function Get_Day (R : Runner_2019; Day_Number : Natural) return Day.Access_Day;
end AOC.AOC_2019;
