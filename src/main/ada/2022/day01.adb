with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with AOC; use AOC;

function Day01 return Solution is
  package Elves_Calories_Vector is new
    Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => Natural);
  package Elves_Calories_Vector_Sorting is new Elves_Calories_Vector.Generic_Sorting;

  Elves_Calories : Elves_Calories_Vector.Vector;
begin
  declare
    F : File_Type;
    Elf_Calories : Natural := 0;
  begin
    Open (F, In_File, "input/2022/day01.txt");
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
      begin
        if Line = "" then
          Elves_Calories.Append (Elf_Calories);
          Elf_Calories := 0;
        else
          Elf_Calories := Elf_Calories + Natural'Value (Line);
        end if;
      end;
    end loop;
    Close (F);
  end;

  Elves_Calories_Vector_Sorting.Sort (Elves_Calories);

  declare
    Top_3_Calories : Natural := (
      Elves_Calories.Last_Element +
      Elves_Calories (Elves_Calories.Last_Index - 1) +
      Elves_Calories (Elves_Calories.Last_Index - 2)
    );
  begin
    return New_Solution (
      Elves_Calories.Last_Element'Image,
      Top_3_Calories'Image
    );
  end;
end Day01;
