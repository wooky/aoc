with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day04 is
  package ID_Range_Package is
    type ID_Range is tagged record
      From: Natural;
      To: Natural;
    end record;

    function Contains (Outer, Inner : ID_Range) return Boolean;
    function Overlaps (Outer, Inner : ID_Range) return Boolean;
  end ID_Range_Package;
  package body ID_Range_Package is
    function Contains (Outer, Inner : ID_Range) return Boolean is
    begin
      return Outer.From <= Inner.From and Outer.To >= Inner.To;
    end Contains;

    function Overlaps (Outer, Inner : ID_Range) return Boolean is
    begin
      return Outer.From <= Inner.To and Outer.To >= Inner.From;
    end Overlaps;
  end ID_Range_Package;
  use ID_Range_Package;

  function To_ID_Range (S : String) return ID_Range is
    Dash : Natural := Index (Source => S, Pattern => "-", From => S'First);
  begin
    return (
      From => Natural'Value (S (S'First .. Dash - 1)),
      To => Natural'Value (S (Dash + 1 .. S'Last))
    );
  end To_ID_Range;

  Fully_Contained_Ranges : Natural := 0;
  Overlapping_Ranges : Natural := 0;
begin
  declare
    F : File_Type;
  begin
    Open (F, In_File, "input/2022/day04.txt");
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
        Comma : Natural := Index (Source => Line, Pattern => ",", From => Line'First);
        Range1 : ID_Range := To_ID_Range (Line (Line'First .. Comma - 1));
        Range2 : ID_Range := To_ID_Range (Line (Comma + 1 .. Line'Last));
      begin
        if Range1.Contains (Range2) or Range2.Contains (Range1) then
          Fully_Contained_Ranges := Fully_Contained_Ranges + 1;
        end if;
        if Overlaps (Range1, Range2) then
          Overlapping_Ranges := Overlapping_Ranges + 1;
        end if;
      end;
    end loop;
    Close (F);
  end;

  Put_Line (Fully_Contained_Ranges'Image);
  Put_Line (Overlapping_Ranges'Image);
end Day04;
