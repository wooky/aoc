with Ada.Text_IO; use Ada.Text_IO;
with AOC; use AOC;
with Day01;
with Day02;
with Day03;
with Day04;
with Day05;
with Day06;

function Run_2022 (Day : Positive) return Solution is
begin
  case Day is
    when 1 => return Day01;
    when 2 => return Day02;
    when 3 => return Day03;
    when 4 => return Day04;
    when 5 => return Day05;
    when 6 => return Day06;
    when others => raise Name_Error with "Invalid day " & Day'Image;
  end case;
end Run_2022;
