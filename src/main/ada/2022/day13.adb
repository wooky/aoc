with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with AOC; use AOC;

function Day13 (F : Aoc_File) return Solution is
  type Comparison is (Unknown, Right_Order, Wrong_Order);

  function Compare (Left, Right : JSON_Array) return Comparison is
    Left_Elements : Natural := Length (Left);
    Right_Elements : Natural := Length (Right);
  begin
    for I in 1 .. Natural'Min (Left_Elements, Right_Elements) loop
      declare
        Sub_Left : JSON_Value := Get (Left, I);
        Sub_Right : JSON_Value := Get (Right, I);
        Result : Comparison := Unknown;
        Wrapper : JSON_Array;
      begin
        if Sub_Left.Kind = JSON_Array_Type and Sub_Right.Kind = JSON_Array_Type then
          Result := Compare (Sub_Left.Get, Sub_Right.Get);
        elsif Sub_Left.Kind = JSON_Array_Type then
          Append (Wrapper, Sub_Right);
          Result := Compare (Sub_Left.Get, Wrapper);
        elsif Sub_Right.Kind = JSON_Array_Type then
          Append (Wrapper, Sub_Left);
          Result := Compare (Wrapper, Sub_Right.Get);
        else
          declare
            Sub_Left_Int : Integer := Get (Sub_Left);
            Sub_Right_Int : Integer := Get (Sub_Right);
          begin
            if Sub_Left_Int < Sub_Right_Int then
              Result := Right_Order;
            elsif Sub_Left_Int > Sub_Right_Int then
              Result := Wrong_Order;
            end if;
          end;
        end if;

        if Result /= Unknown then
          return Result;
        end if;
      end;
    end loop;

    if Left_Elements < Right_Elements then
      return Right_Order;
    elsif Left_Elements > Right_Elements then
      return Wrong_Order;
    end if;

    return Unknown;
  end Compare;

  function "<" (Left, Right : JSON_Array) return Boolean is
  begin
    return Compare (Left, Right) = Right_Order;
  end "<";

  package Packet_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => JSON_Array);
  package Packet_Vector_Sorting is new Packet_Vector.Generic_Sorting;

  Divider_Packet_1 : constant JSON_Array := Get (Read ("[[2]]"));
  Divider_Packet_2 : constant JSON_Array := Get (Read ("[[6]]"));

  Pairs_In_Right_Order : Natural := 0;
  Packets : Packet_Vector.Vector;
begin
  declare
    Pair : Positive := 1;
  begin
    while not End_Of_File (F) loop
      declare
        Left : JSON_Array := Get (Read (Get_Line (F)));
        Right : JSON_Array := Get (Read (Get_Line (F)));
        Blank : String := Get_Line (F);

        Result : Comparison := Compare (Left, Right);
      begin
        case Result is
          when Right_Order => Pairs_In_Right_Order := Pairs_In_Right_Order + Pair;
          when Wrong_Order => null;
          when Unknown => raise Program_Error with "Comparison of pair " & Pair'Image & " is unknown";
        end case;

        Packets.Append (Left);
        Packets.Append (Right);
      end;
      Pair := Pair + 1;
    end loop;
  end;

  Packets.Append (Divider_Packet_1);
  Packets.Append (Divider_Packet_2);
  Packet_Vector_Sorting.Sort (Packets);

  declare
    Idx1 : Positive := Packets.Find_Index (Divider_Packet_1);
    Idx2 : Positive := Packets.Find_Index (Divider_Packet_2);
    Decoder_Key : Positive := Idx1 * Idx2;
  begin
    return New_Solution (S1 => Pairs_In_Right_Order'Image, S2 => Decoder_Key'Image);
  end;
end Day13;
