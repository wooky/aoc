with Ada.Containers; use Ada.Containers;
with AOC; use AOC;

package AOC.Coordinates is
  pragma Preelaborate;

  type Coordinate is tagged record
    Row, Column : Integer := 0;
  end record;

  type Direct_Neighbors_Array is array(1 .. 4) of Coordinate;

  function Hash (C : Coordinate) return Hash_Type;

  procedure Translate (C : in out Coordinate ; Delta_Row, Delta_Column : Integer);
  function Translate (C : Coordinate ; Delta_Row, Delta_Column : Integer) return Coordinate;
  function Direct_Neighbors (C : Coordinate) return Direct_Neighbors_Array;
  function Distance (From, To : Coordinate) return Natural;

  function Find (Where : Rectangle ; What : Character) return Coordinate;
private
end AOC.Coordinates;
