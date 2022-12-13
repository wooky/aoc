with Ada.Text_IO; use Ada.Text_IO;
with AOC; use AOC;

function Day10 return Solution is
  type Unformatted_CRT_Display is array(1 .. 40 * 6) of Character;

  type CPU_Parameters is record
    Signal_Strength : Natural := 0;
    Cycle : Natural := 0;
    Cycles_To_Wait : Natural := 0;
    Target_Cycle : Positive := 20;
    X : Integer := 1;
    CRT : Unformatted_CRT_Display;
  end record;

  procedure Wait_Cycles (CPU: in out CPU_Parameters) is
  begin
    while CPU.Cycles_To_Wait /= 0 loop
      if CPU.Cycle mod 40 in CPU.X - 1 .. CPU.X + 1 then
        CPU.CRT (CPU.Cycle + 1) := '#';
      else
        CPU.CRT (CPU.Cycle + 1) := '.';
      end if;

      CPU.Cycles_To_Wait := CPU.Cycles_To_Wait - 1;
      CPU.Cycle := CPU.Cycle + 1;
      if CPU.Cycle = CPU.Target_Cycle then
        CPU.Signal_Strength := CPU.Signal_Strength + CPU.Target_Cycle * CPU.X;
        CPU.Target_Cycle := CPU.Target_Cycle + 40;
      end if;
    end loop;
  end Wait_Cycles;

  CPU : CPU_Parameters;
begin
  declare
    F : File_Type;
  begin
    Open (F, In_File, "input/2022/day10.txt");
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
      begin
        if Line = "noop" then
          CPU.Cycles_To_Wait := 1;
        else
          CPU.Cycles_To_Wait := 2;
        end if;

        Wait_Cycles (CPU);

        if Line /= "noop" then
          declare
            V : Integer := Integer'Value (Line (6 .. Line'Last));
          begin
            CPU.X := CPU.X + V;
          end;
        end if;
      end;
    end loop;
    Close (F);
  end;
  Wait_Cycles (CPU);

  declare
    type Formatted_CRT_Display is array(1 .. 41 * 6) of Character;
    CRT : Formatted_CRT_Display;
  begin
    for Row in 0 .. 5 loop
      for Column in 1 .. 40 loop
        CRT (Row * 41 + Column) := CPU.CRT (Row * 40 + Column);
      end loop;
      CRT (Row * 41 + 41) := Character'Val (10);
    end loop;
    return New_Solution (S1 => CPU.Signal_Strength'Image, S2 => String (CRT));
  end;
end Day10;
