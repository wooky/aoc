with Ada.Containers.Ordered_Sets;
with AOC; use AOC;

function Day03 (F : Aoc_File) return Solution is
  package Contents_Container is new
    Ada.Containers.Ordered_Sets (Element_Type => Character);
  subtype Contents is Contents_Container.Set;

  function Get_Contents (S : String) return Contents is
    Result : Contents;
  begin
    for C of S loop
      Result.Include (C);
    end loop;
    return Result;
  end Get_Contents;

  type Priority is new Natural;
  function Get_Priority (C : Character) return Priority is
  begin
    case C is
      when 'a' .. 'z' => return Character'Pos (C) - Character'Pos ('a') + 1;
      when 'A' .. 'Z' => return Character'Pos (C) - Character'Pos ('A') + 27;
      when others => raise Program_Error with "Invalid character " & C;
    end case;
  end Get_Priority;

  Total_Compartment_Priority : Priority := 0;
  Total_Group_Priority : Priority := 0;
begin
  declare
    Group_Contents : array (1 .. 3) of Contents;
    Group_Contents_Idx : Positive := Group_Contents'First;
  begin
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
        Half : Natural := Line'Length / 2;
        Left_Compartment : Contents := Get_Contents (Line (Line'First .. Half));
        Right_Compartment : Contents := Get_Contents (Line (Half + 1 .. Line'Last));
      begin
        Left_Compartment.Intersection (Right_Compartment);
        Total_Compartment_Priority := Total_Compartment_Priority + Get_Priority (Left_Compartment.First_Element);

        Group_Contents (Group_Contents_Idx) := Get_Contents (Line);
        if Group_Contents_Idx = Group_Contents'Last then
          Group_Contents (Group_Contents'First).Intersection (Group_Contents (2));
          Group_Contents (Group_Contents'First).Intersection (Group_Contents (3));
          Total_Group_Priority := Total_Group_Priority + Get_Priority (Group_Contents (Group_Contents'First).First_Element);
          Group_Contents_Idx := Group_Contents'First;
        else
          Group_Contents_Idx := Group_Contents_Idx + 1;
        end if;
      end;
    end loop;
  end;

  return New_Solution (Total_Compartment_Priority'Image, Total_Group_Priority'Image);
end Day03;
