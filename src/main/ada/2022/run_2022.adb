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

function Run_2022 (File : Aoc_File ; Day : Positive) return Solution is
begin
  case Day is
    when 1 => return Day01 (File);
    when 2 => return Day02 (File);
    when 3 => return Day03 (File);
    when 4 => return Day04 (File);
    when 5 => return Day05 (File);
    when 6 => return Day06 (File);
    when 7 => return Day07 (File);
    when 8 => return Day08 (File);
    when 9 => return Day09 (File);
    when 10 => return Day10 (File);
    when 11 => return Day11 (File);
    when others => raise Program_Error with "Invalid day " & Day'Image;
  end case;
end Run_2022;
