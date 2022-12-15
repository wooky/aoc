with Ada.Containers; use Ada.Containers;

package AOC.Coordinates is
  pragma Preelaborate;

  type Coordinate is tagged record
    Row, Column : Integer := 0;
  end record;

  function Hash (C : Coordinate) return Hash_Type;

  procedure Translate (C : in out Coordinate ; Delta_Row, Delta_Column : Integer);
  function Translate (C : Coordinate ; Delta_Row, Delta_Column : Integer) return Coordinate;
private
end AOC.Coordinates;
