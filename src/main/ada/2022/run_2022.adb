with Ada.Text_IO; use Ada.Text_IO;
with Day01;
with Day02;
with Day03;
with Day04;
with Day05;

procedure Run_2022 (Day : Positive) is
begin
  case Day is
    when 1 => Day01;
    when 2 => Day02;
    when 3 => Day03;
    when 4 => Day04;
    when 5 => Day05;
    when others => raise Name_Error with "Invalid day " & Day'Image;
  end case;
end Run_2022;
