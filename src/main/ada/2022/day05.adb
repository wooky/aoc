with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;

procedure Day05 is
  type FSM is (Populate, Skip, Operate);

  package Container_Stack_Vector_Package is new
    Ada.Containers.Vectors(
      Index_Type => Natural,
      Element_Type => Character
    );
  package Container_Stacks_Vector_Package is new
    Ada.Containers.Vectors(
      Index_Type => Positive,
      Element_Type => Container_Stack_Vector_Package.Vector,
      "=" => Container_Stack_Vector_Package."="
    );
  subtype Container_Stacks_Vector is Container_Stacks_Vector_Package.Vector;

  function Stacks_Top (CSV : Container_Stacks_Vector) return String is
    S : String (CSV.First_Index .. CSV.Last_Index);
  begin
    for I in CSV.First_Index .. CSV.Last_Index loop
      S (I) := CSV (I).Last_Element;
    end loop;
    return S;
  end Stacks_Top;

  Container_Stacks : Container_Stacks_Vector;
  Container_Stacks_9001 : Container_Stacks_Vector;
begin
  declare
    F : File_Type;
    FSM_State : FSM := Populate;
    Operation_Regex : Pattern_Matcher := Compile ("move (\d+) from (\d+) to (\d+)");
    Operation_Matches : Match_Array (1 .. 3);
  begin
    Open (F, In_File, "input/2022/day05.txt");
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
      begin
        case FSM_State is
          when Populate =>
            if Container_Stacks.Length = 0 then
              Container_Stacks.Set_Length ((Line'Length + 1) / 4);
            end if;
            if Line (Line'First + 1) = '1' then
              for CS of Container_Stacks loop
                CS.Reverse_Elements;
              end loop;
              Container_Stacks_9001 := Container_Stacks.Copy;
              FSM_State := Skip;
            else
              declare
                Container_Idx : Natural := Container_Stacks.First_Index;
                Line_Idx : Natural := Line'First + 1;
              begin
                while Line_Idx < Line'Last loop
                  if Line (Line_Idx) /= ' ' then
                    Container_Stacks (Container_Idx).Append (Line (Line_Idx));
                  end if;
                  Container_Idx := Container_Idx + 1;
                  Line_Idx := Line_Idx + 4;
                end loop;
              end;
            end if;
          when Skip =>
            FSM_State := Operate;
          when Operate =>
            Match (Operation_Regex, Line, Operation_Matches);
            declare
              Moves : Positive := Positive'Value (Line (Operation_Matches (1).First .. Operation_Matches (1).Last));
              From : Positive := Positive'Value (Line (Operation_Matches (2).First .. Operation_Matches (2).Last));
              To : Positive := Positive'Value (Line (Operation_Matches (3).First .. Operation_Matches (3).Last));

              Idx_9001 : Natural := Container_Stacks_9001 (From).Last_Index - Moves + 1;
            begin
              for I in 1 .. Moves loop
                Container_Stacks (To).Append (Container_Stacks (From).Last_Element);
                Container_Stacks (From).Delete_Last;

                Container_Stacks_9001 (To).Append (Container_Stacks_9001 (From)(Idx_9001));
                Container_Stacks_9001 (From).Delete (Idx_9001);
              end loop;
            end;
        end case;
      end;
    end loop;
    Close (F);
  end;

  Put_Line (Stacks_Top (Container_Stacks));
  Put_Line (Stacks_Top (Container_Stacks_9001));
end Day05;
