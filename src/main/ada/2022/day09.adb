with Ada.Containers.Ordered_Sets;
with AOC; use AOC;

function Day09 (F : Aoc_File) return Solution is
  type Coordinate is record
    X, Y : Integer := 0;
  end record;

  function "<" (L, R : Coordinate) return Boolean is
  begin
    if L.X = R.X then
      return L.Y < R.Y;
    end if;
    return L.X < R.X;
  end "<";

  procedure Move_Head_Coord (Head_Coord : in out Coordinate ; Direction : Character) is
  begin
    case Direction is
      when 'U' => Head_Coord.Y := Head_Coord.Y + 1;
      when 'D' => Head_Coord.Y := Head_Coord.Y - 1;
      when 'L' => Head_Coord.X := Head_Coord.X - 1;
      when 'R' => Head_Coord.X := Head_Coord.X + 1;
      when others => raise Program_Error with "Invalid direction " & Direction;
    end case;
  end Move_Head_Coord;

  procedure Move_Subsequent_Coord (Subsequent_Coord : in out Coordinate ; Head_Coord : Coordinate) is
    Delta_X : Integer := Head_Coord.X - Subsequent_Coord.X;
    Delta_Y : Integer := Head_Coord.Y - Subsequent_Coord.Y;
  begin
    if abs Delta_X <= 1 and abs Delta_Y <= 1 then
      return;
    end if;
    if Delta_X /= 0 then
      Subsequent_Coord.X := Subsequent_Coord.X + Delta_X / abs Delta_X;
    end if;
    if Delta_Y /= 0 then
      Subsequent_Coord.Y := Subsequent_Coord.Y + Delta_Y / abs Delta_Y;
    end if;
  end Move_Subsequent_Coord;

  package Tail_Visit_Set is new Ada.Containers.Ordered_Sets (
    Element_Type => Coordinate
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
