with Ada.Containers.Hashed_Sets;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with AOC; use AOC;
with AOC.Coordinates; use AOC.Coordinates;

function Day14 (F : Aoc_File) return Solution is
  type Coordinate_Extraction_Result is record
    Coord : Coordinate;
    Next_From : Natural;
  end record;

  function Extract_Coordinate (Line : String; From : Natural) return Coordinate_Extraction_Result is
    Comma : Positive := Index (Line, ",", From);
    To : Natural := Index (Line, " ", From);
    Next_From : Natural;
  begin
    if To = 0 then
      To := Line'Last;
      Next_From := 0;
    else
      To := To - 1;
      Next_From := To + 5;
    end if;
    declare
      Column : Positive := Positive'Value (Line (From .. Comma - 1));
      Row : Positive := Positive'Value (Line (Comma + 1 .. To));
    begin
      return (
        Coord => (Row => Row, Column => Column),
        Next_From => Next_From
      );
    end;
  end Extract_Coordinate;

  package Solids_Container is new Ada.Containers.Hashed_Sets (
    Element_Type => Coordinate,
    Hash => Hash,
    Equivalent_Elements => "="
  );
  subtype Solids_Set is Solids_Container.Set;

  Solids : Solids_Set;
  Highest_Row : Natural := 0;
  S1, S2 : Natural := 0;
begin
  while not End_Of_File (F) loop
    declare
      Line : String := Get_Line (F);
      Prev : Coordinate_Extraction_Result := Extract_Coordinate (Line, Line'First);
    begin
      loop
        declare
          Curr : Coordinate_Extraction_Result := Extract_Coordinate (Line, Prev.Next_From);
          Coord : Coordinate := Prev.Coord;
        begin
          loop
            Solids.Include (Coord);
            Highest_Row := Natural'Max (Highest_Row, Coord.Row);
            exit when Coord = Curr.Coord;
            if Curr.Coord.Row /= Prev.Coord.Row then
              Coord.Translate ((Curr.Coord.Row - Prev.Coord.Row) / abs (Curr.Coord.Row - Prev.Coord.Row), 0);
            elsif Curr.Coord.Column /= Prev.Coord.Column then
              Coord.Translate (0, (Curr.Coord.Column - Prev.Coord.Column) / abs (Curr.Coord.Column - Prev.Coord.Column));
            end if;
          end loop;
          exit when Curr.Next_From = 0;
          Prev := Curr;
        end;
      end loop;
    end;
  end loop;

  declare
    Solids_And_Sand : Solids_Set := Solids;
  begin
    Outer1: loop
      declare
        Coord : Coordinate := (Row => 0, Column => 500);
      begin
        loop
          exit Outer1 when Coord.Row = Highest_Row;
          if not Solids_And_Sand.Contains (Coord.Translate (1, 0)) then
            Coord.Translate (1, 0);
          elsif not Solids_And_Sand.Contains (Coord.Translate (1, -1)) then
            Coord.Translate (1, -1);
          elsif not Solids_And_Sand.Contains (Coord.Translate (1, 1)) then
            Coord.Translate (1, 1);
          else
            Solids_And_Sand.Insert (Coord);
            S1 := S1 + 1;
            exit;
          end if;
        end loop;
      end;
    end loop Outer1;
  end;

  declare
    Solids_And_Sand : Solids_Set := Solids;
  begin
    Outer2: loop
      declare
        Coord : Coordinate := (Row => 0, Column => 500);
      begin
        loop
          if Coord.Row = Highest_Row + 1 then
            Solids_And_Sand.Insert (Coord);
            S2 := S2 + 1;
            exit;
          elsif not Solids_And_Sand.Contains (Coord.Translate (1, 0)) then
            Coord.Translate (1, 0);
          elsif not Solids_And_Sand.Contains (Coord.Translate (1, -1)) then
            Coord.Translate (1, -1);
          elsif not Solids_And_Sand.Contains (Coord.Translate (1, 1)) then
            Coord.Translate (1, 1);
          else
            Solids_And_Sand.Insert (Coord);
            S2 := S2 + 1;
            exit Outer2 when Coord.Row = 0;
            exit;
          end if;
        end loop;
      end;
    end loop Outer2;
  end;
  
  return New_Solution (S1 => S1'Image, S2 => S2'Image);
end Day14;
