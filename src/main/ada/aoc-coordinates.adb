package body AOC.Coordinates is
  function Hash (C : Coordinate) return Hash_Type is
  begin
    return Hash_Type (abs ((C.Row + C.Column + 1) * (C.Row + C.Column) / 2 + C.Row));
  end Hash;

  procedure Translate (C : in out Coordinate ; Delta_Row, Delta_Column : Integer) is
  begin
    C.Row := C.Row + Delta_Row;
    C.Column := C.Column + Delta_Column;
  end Translate;

  function Translate (C : Coordinate ; Delta_Row, Delta_Column : Integer) return Coordinate is
    Result : Coordinate := C;
  begin
    Result.Translate (Delta_Row, Delta_Column);
    return Result;
  end Translate;

  function Direct_Neighbors (C : Coordinate) return Direct_Neighbors_Array is
  begin
    return (
      C.Translate (-1, 0),
      C.Translate (1, 0),
      C.Translate (0, -1),
      C.Translate (0, 1)
    );
  end Direct_Neighbors;

  function Distance (From, To : Coordinate) return Natural is
  begin
    return abs (From.Row - To.Row) + abs (From.Column - To.Column);
  end Distance;

  function Find (Where : Rectangle ; What : Character) return Coordinate is
  begin
    for Row in Where'Range(1) loop
      for Column in Where'Range(2) loop
        if Where (Row, Column) = What then
          return (Row => Row, Column => Column);
        end if;
      end loop;
    end loop;
    raise Program_Error with "Not found!";
  end Find;

end AOC.Coordinates;
