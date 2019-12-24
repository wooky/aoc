with AOC.AOC_2019.Day01;
with AOC.AOC_2019.Day02;
with AOC.AOC_2019.Day03;
with AOC.AOC_2019.Day05;
with AOC.AOC_2019.Day06;
with AOC.AOC_2019.Day07;
with AOC.AOC_2019.Day08;

package body AOC.AOC_2019 is
   function Get_Day (R : Runner_2019; Day_Number : Natural) return Day.Access_Day is
   begin
      return (case Day_Number is
                 when 1 => Day01.Create,
                 when 2 => new Day02.Day_02,
                 when 3 => new Day03.Day_03,
                 when 5 => new Day05.Day_05,
                 when 6 => new Day06.Day_06,
                 when 7 => new Day07.Day_07,
                 when 8 => new Day08.Day_08,
                 when others => null);
   end Get_Day;
end AOC.AOC_2019;
