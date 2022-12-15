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

end AOC.Coordinates;
