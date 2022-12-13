with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body AOC is
  function New_Solution (S1, S2 : String) return Solution is
  begin
    return (S1 => New_String (S1), S2 => New_String (S2));
  end New_Solution;

  function New_File (Input : chars_ptr) return Aoc_File is
  begin
    return new Aoc_File_Record'(Input => String_Holder.To_Holder (Value (Input)), others => <>);
  end New_File;

  function End_Of_File (File : Aoc_File) return Boolean is
  begin
    return File.Eof;
  end End_Of_File;

  function Get_Line (File : Aoc_File) return String
  is
    Input : String := String_Holder.Element (File.Input);
    To : Natural := Index (Input, "" & Character'Val(10), File.Pos);
  begin
    pragma Assert (not File.Eof);
    if To = 0 then
      declare
        Line : String (1 .. Input'Last - File.Pos + 1);
      begin
        Line := Input (File.Pos .. Input'Last);
        File.Eof := True;
        return Line;
      end;
    end if;
    declare
      Line : String (1 .. To - File.Pos);
    begin
      Line := Input (File.Pos .. To - 1);
      File.Pos := To + 1;
      return Line;
    end;
  end Get_Line;

  procedure Reset (File : Aoc_File) is
  begin
    File.Pos := 1;
    File.Eof := False;
  end Reset;
end AOC;
