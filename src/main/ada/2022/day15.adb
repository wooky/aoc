with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with GNAT.Regpat; use GNAT.Regpat;
with AOC; use AOC;
with AOC.Coordinates; use AOC.Coordinates;

function Day15 (F : Aoc_File) return Solution is
  type Scan_Result is record
    Sensor : Coordinate;
    Beacon : Coordinate;
    Empty_Distance : Natural;
  end record;

  type Column_Range is record
    Left, Right : Integer;
  end record;

  package Scan_Results_Container is new Ada.Containers.Vectors (
    Index_Type => Natural,
    Element_Type => Scan_Result
  );
  subtype Scan_Results_Vector is Scan_Results_Container.Vector;

  package Beacon_Columns_Container is new Ada.Containers.Ordered_Sets (
    Element_Type => Integer
  );
  subtype Beacon_Columns_Set is Beacon_Columns_Container.Set;

  package Column_Ranges_Container is new Ada.Containers.Vectors (
    Index_Type => Natural,
    Element_Type => Column_Range
  );
  subtype Column_Ranges_Vector is Column_Ranges_Container.Vector;

  type Ranges_And_Beacons is record
    Ranges : Column_Ranges_Vector;
    Beacons : Natural := 0;
  end record;

  function Compute_Ranges_And_Beacons (
    Scan_Results : Scan_Results_Vector;
    Row : Natural;
    Clamp_Left, Clamp_Right : Integer
  ) return Ranges_And_Beacons is
    Ranges : Column_Ranges_Vector;
    Beacons_Present : Beacon_Columns_Set;
  begin
    for Scan_Result of Scan_Results loop
      if abs (Scan_Result.Sensor.Row - Row) <= Scan_Result.Empty_Distance then
        declare
          Width : Natural := Scan_Result.Empty_Distance - abs (Scan_Result.Sensor.Row - Row);
          Left : Integer := Scan_Result.Sensor.Column - Width;
          Right : Integer := Scan_Result.Sensor.Column + Width;
        begin
          Ranges.Append((
            Left => Integer'Max (Left, Clamp_Left),
            Right => Integer'Min (Right, Clamp_Right)
          ));
          if Scan_Result.Beacon.Row = Row and Scan_Result.Beacon.Column in Left .. Right then
            Beacons_Present.Include (Scan_Result.Beacon.Column);
          end if;
        end;
      end if;
    end loop;
    
    Outer: loop
      declare
        Victim : Integer := Ranges.Last_Index;
        Target : Integer := Ranges.First_Index;
      begin
        loop
          exit Outer when Victim = Target;
          if
            Ranges (Victim).Left in Ranges (Target).Left .. Ranges (Target).Right or else
            Ranges (Victim).Right in Ranges (Target).Left .. Ranges (Target).Right or else
            Ranges (Target).Left in Ranges (Victim).Left .. Ranges (Victim).Right or else
            Ranges (Target).Right in Ranges (Victim).Left .. Ranges (Victim).Right
          then
            Ranges (Target).Left := Integer'Min (Ranges (Target).Left, Ranges (Victim).Left);
            Ranges (Target).Right := Integer'Max (Ranges (Target).Right, Ranges (Victim).Right);
            Ranges.Delete (Victim);
            exit;
          end if;
          Target := Target + 1;
        end loop;
      end;
    end loop Outer;

    return (Ranges => Ranges, Beacons => Natural (Beacons_Present.Length));
  end Compute_Ranges_And_Beacons;

  Line_Regex : Pattern_Matcher := Compile ("Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)");
  Scan_Results : Scan_Results_Vector;
  S1 : Positive;
  S2 : Long_Long_Integer;
begin
  while not End_Of_File (F) loop
    declare
      Line : String := Get_Line (F);
      Matches : Match_Array (1 .. 4);
    begin
      Match (Line_Regex, Line, Matches);
      declare
        Sensor : Coordinate := (
          Row => Integer'Value (Line (Matches (2).First .. Matches (2).Last)),
          Column => Integer'Value (Line (Matches (1).First .. Matches (1).Last))
        );
        Beacon : Coordinate := (
          Row => Integer'Value (Line (Matches (4).First .. Matches (4).Last)),
          Column => Integer'Value (Line (Matches (3).First .. Matches (3).Last))
        );
        Empty_Distance : Natural := Distance (Sensor, Beacon);
      begin
        Scan_Results.Append ((
          Sensor => Sensor,
          Beacon => Beacon,
          Empty_Distance => Empty_Distance
        ));
      end;
    end;
  end loop;

  declare
    RAB : Ranges_And_Beacons := Compute_Ranges_And_Beacons (Scan_Results, 2000000, Integer'First, Integer'Last);
  begin
    pragma Assert (RAB.Ranges.Length = 1);
    S1 := RAB.Ranges.First_Element.Right - RAB.Ranges.First_Element.Left - RAB.Beacons + 1;
  end;

  for Row in reverse 0 .. 4000000 loop
    declare
      RAB : Ranges_And_Beacons := Compute_Ranges_And_Beacons (Scan_Results, Row, 0, 4000000);
    begin
      if RAB.Ranges.Length = 2 then
        declare
          Gap : Positive;
        begin
          if RAB.Ranges (0).Right < RAB.Ranges (1).Left then
            Gap := RAB.Ranges (0).Right + 1;
          else
            Gap := RAB.Ranges (1).Right + 1;
          end if;
          S2 := Long_Long_Integer (Gap) * 4000000 + Long_Long_Integer (Row);
          exit;
        end;
      end if;
    end;
  end loop;

  return New_Solution (S1 => S1'Image, S2 => S2'Image);
end Day15;
