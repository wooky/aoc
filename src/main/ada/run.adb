with Ada.Text_IO; use Ada.Text_IO;
with Run_2022;

function Run (Year, Day : Positive) return Solution is
begin
  case Year is
    when 2022 => return Run_2022 (Day);
    when others => raise Name_Error with "Invalid year " & Year'Image;
  end case;
end Run;
