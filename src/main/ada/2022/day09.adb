with Ada.Containers.Hashed_Sets;
with AOC; use AOC;
with AOC.Coordinates; use AOC.Coordinates;

function Day09 (F : Aoc_File) return Solution is
  procedure Move_Head_Coord (Head_Coord : in out Coordinate ; Direction : Character) is
  begin
    case Direction is
      when 'U' => Head_Coord.Translate (1, 0);
      when 'D' => Head_Coord.Translate (-1, 0);
      when 'L' => Head_Coord.Translate (0, -1);
      when 'R' => Head_Coord.Translate (0, 1);
      when others => raise Program_Error with "Invalid direction " & Direction;
    end case;
  end Move_Head_Coord;

  procedure Move_Subsequent_Coord (Subsequent_Coord : in out Coordinate ; Head_Coord : Coordinate) is
    Delta_X : Integer := Head_Coord.Column - Subsequent_Coord.Column;
    Delta_Y : Integer := Head_Coord.Row - Subsequent_Coord.Row;
  begin
    if abs Delta_X <= 1 and abs Delta_Y <= 1 then
      return;
    end if;
    if Delta_X /= 0 then
      Subsequent_Coord.Translate (0, Delta_X / abs Delta_X);
    end if;
    if Delta_Y /= 0 then
      Subsequent_Coord.Translate(Delta_Y / abs Delta_Y, 0);
    end if;
  end Move_Subsequent_Coord;

  package Tail_Visit_Set is new Ada.Containers.Hashed_Sets (
    Element_Type => Coordinate,
    Hash => Hash,
    Equivalent_Elements => "="
  );

  type Knot_Coordinates is array(Positive range <>) of Coordinate;

  type Rope (Knot_Count : Positive) is record
    Knots : Knot_Coordinates (1 .. Knot_Count);
    Tail_Visits : Tail_Visit_Set.Set;
  end record;

  procedure Move_Rope (R : in out Rope ; Direction : Character) is
  begin
    Move_Head_Coord (R.Knots (R.Knots'First), Direction);
    for I in R.Knots'First + 1 .. R.Knots'Last loop
      Move_Subsequent_Coord (R.Knots (I), R.Knots (I - 1));
    end loop;
    R.Tail_Visits.Include (R.Knots (R.Knots'Last));
  end Move_Rope;

  Rope2 : Rope (2);
  Rope10 : Rope (10);
begin
  declare
  begin
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
        Direction : Character := Line (Line'First);
        Moves : Positive := Positive'Value (Line (3 .. Line'Last));
      begin
        for I in 1 .. Moves loop
          Move_Rope (Rope2, Direction);
          Move_Rope (Rope10, Direction);
        end loop;
      end;
    end loop;
  end;
  return New_Solution (S1 => Rope2.Tail_Visits.Length'Image, S2 => Rope10.Tail_Visits.Length'Image);
end Day09;
