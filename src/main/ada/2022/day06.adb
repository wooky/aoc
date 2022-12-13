with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with AOC; use AOC;

function Day06 (F : Aoc_File) return Solution is
  function Find_Marker (S : String ; Size : Positive) return Positive is
    package Character_Set is new Ada.Containers.Ordered_Sets(
      Element_Type => Character
    );
  begin
    for I in S'First + Size - 1 .. S'Last loop
      declare
        Group_Chars : Character_Set.Set;
      begin
        for J in I - Size + 1 .. I loop
          Group_Chars.Include (S (J));
        end loop;
        if Positive (Group_Chars.Length) = Size then
          return I;
        end if;
      end;
    end loop;
    raise Program_Error with "Find_Marker failed!";
  end Find_Marker;

  function Read_File return String is
  begin
    declare
      Line : String := Get_Line (F);
    begin
      return Line;
    end;
  end Read_File;

  File : String := Read_File;
begin
  return New_Solution (
    S1 => Find_Marker (File, 4)'Image,
    S2 => Find_Marker (File, 14)'Image
  );
end Day06;
