with AOC.AOC_2019.Day01;

package body AOC.AOC_2019 is
   function Get_Day (R : Runner_2019; Day_Number : Natural) return Day.Access_Day is
   begin
      return (case Day_Number is
                 when 1 => new Day01.Day_01,
                 when others => null);
   end Get_Day;
end AOC.AOC_2019;
