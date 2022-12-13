with Ada.Text_IO; use Ada.Text_IO;
with AOC; use AOC;
with Day01;
with Day02;
with Day03;
with Day04;
with Day05;
with Day06;
with Day07;
with Day08;
with Day09;
with Day10;
with Day11;

function Run_2022 (Day : Positive) return Solution is
begin
  case Day is
    when 1 => return Day01;
    when 2 => return Day02;
    when 3 => return Day03;
    when 4 => return Day04;
    when 5 => return Day05;
    when 6 => return Day06;
    when 7 => return Day07;
    when 8 => return Day08;
    when 9 => return Day09;
    when 10 => return Day10;
    when 11 => return Day11;
    when others => raise Name_Error with "Invalid day " & Day'Image;
  end case;
end Run_2022;
