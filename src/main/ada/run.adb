with Interfaces.C.Strings; use Interfaces.C.Strings;
with AOC; use AOC;
with Run_2022;

function Run (File_C : chars_ptr; Year, Day : Positive) return Solution is
  File : Aoc_File := New_File (File_C);
begin
  case Year is
    when 2022 => return Run_2022 (File, Day);
    when others => raise Program_Error with "Invalid year " & Year'Image;
  end case;
end Run;
