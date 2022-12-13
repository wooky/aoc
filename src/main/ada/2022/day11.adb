with Ada.Containers; use Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with AOC; use AOC;

function Day11 return Solution is
  package Item_List_Vector is new Ada.Containers.Vectors (
    Index_Type => Natural,
    Element_Type => Long_Long_Integer
  );
  subtype Item_List is Item_List_Vector.Vector;

  type Operand is (Add, Multiply, Square);

  type Monkey is record
    Items : Item_List;
    Op : Operand;
    Op_Scalar : Natural;
    Test_Divisible : Positive;
    Test_True : Natural;
    Test_False : Natural;
    Items_Inspected : Natural := 0;
  end record;

  package Monkey_Vector_Type is new Ada.Containers.Vectors (
    Index_Type => Natural,
    Element_Type => Monkey
  );
  subtype Monkey_Vector is Monkey_Vector_Type.Vector;

  function Keep_Away (Monkeys_Original: Monkey_Vector; Rounds: Positive; Worry_Divide: Long_Long_Integer) return Long_Long_Integer is
    Monkeys : Monkey_Vector := Monkeys_Original;
  begin
    for Round in 1 .. Rounds loop
      for M of Monkeys loop
        while not M.Items.Is_Empty loop
          declare
            Worry : Long_Long_Integer := M.Items.First_Element;
          begin
            M.Items.Delete_First;
            case M.Op is
              when Add => Worry := Worry + Long_Long_Integer (M.Op_Scalar);
              when Multiply => Worry := Worry * Long_Long_Integer (M.Op_Scalar);
              when Square => Worry := Worry * Worry;
            end case;
            Worry := Worry / Worry_Divide;
            Worry := Worry mod (2 * 3 * 5 * 7 * 11 * 13 * 17 * 19);
            if Worry mod Long_Long_Integer (M.Test_Divisible) = 0 then
              Monkeys (M.Test_True).Items.Append (Worry);
            else
              Monkeys (M.Test_False).Items.Append (Worry);
            end if;
            M.Items_Inspected := M.Items_Inspected + 1;
          end;
        end loop;
      end loop;
    end loop;

    declare
      type Inspection_Array is array(Natural range <>) of Long_Long_Integer;
      Inspections : Inspection_Array(Monkeys.First_Index .. Monkeys.Last_Index);
      procedure Array_Sort is new Ada.Containers.Generic_Array_Sort (Index_Type => Natural, Element_Type => Long_Long_Integer, Array_Type => Inspection_Array);
    begin
      for I in Inspections'Range loop
        Inspections (I) := Long_Long_Integer (Monkeys (I).Items_Inspected);
      end loop;
      Array_Sort (Inspections);
      return Inspections (Inspections'Last - 1) * Inspections (Inspections'Last);
    end;
  end Keep_Away;

  Monkeys : Monkey_Vector;
begin
  declare
    F : File_Type;
  begin
    Open (F, In_File, "input/2022/day11.txt");
    while not End_Of_File (F) loop
      declare
        Line_Monkey : String := Get_Line (F);
        Line_Starting_Items : String := Get_Line (F);
        Line_Operation : String := Get_Line (F);
        Line_Test : String := Get_Line (F);
        Line_If_True : String := Get_Line (F);
        Line_If_False : String := Get_Line (F);
        Line_Blank : String := Get_Line (F);

        M : Monkey;
        From : Positive;
        To : Natural;
      begin
        -- Parse starting items
        From := 19;
        loop
          To := Index (Line_Starting_Items, ",", From);
          if To = 0 then
            M.Items.Append (Long_Long_Integer'Value (Line_Starting_Items (From .. Line_Starting_Items'Last)));
            exit;
          end if;
          M.Items.Append (Long_Long_Integer'Value (Line_Starting_Items (From .. To - 1)));
          From := To + 2;
        end loop;

        -- Parse operands
        From := Index (Line_Operation, " ", Line_Operation'Last, Backward) + 1;
        if Line_Operation (From .. Line_Operation'Last) = "old" then
          M.Op := Square;
        else
          M.Op_Scalar := Positive'Value (Line_Operation (From .. Line_Operation'Last));
          case Line_Operation (From - 2) is
            when '+' => M.Op := Add;
            when '*' => M.Op := Multiply;
            when others => raise Program_Error with "Invalid operand " & Line_Operation (From - 2);
          end case;
        end if;

        -- Parse tests
        From := Index (Line_Test, " ", Line_Test'Last, Backward) + 1;
        M.Test_Divisible := Positive'Value (Line_Test (From .. Line_Test'Last));
        M.Test_True := Natural'Value (Line_If_True (Line_If_True'Last .. Line_If_True'Last));
        M.Test_False := Natural'Value (Line_If_False (Line_If_False'Last .. Line_If_False'Last));

        -- Add monkey
        Monkeys.Append (M);
      end;
    end loop;
    Close (F);
  end;

  return New_Solution (
    S1 => Keep_Away (Monkeys, 20, 3)'Image,
    S2 => Keep_Away (Monkeys, 10000, 1)'Image
  );
end Day11;
