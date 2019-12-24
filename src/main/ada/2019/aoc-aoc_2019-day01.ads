package AOC.AOC_2019.Day01 is
   type Day_01 is new Day.Day with private;

   overriding procedure Init (D : in out Day_01; Root : String);
   overriding function Part_1 (D : Day_01) return String;
   overriding function Part_2 (D : Day_01) return String;
private
   type Mass is new Natural;
   subtype Fuel is Mass;

   type Day_01 is new Day.Day with record
      Total_Fuel : Fuel := 0;
      Abhorrent_Total_Fuel : Fuel := 0;
   end record;

   function Calc_Fuel (M : Mass) return Fuel;
   function Calc_Abhorrent_Fuel (M : Mass) return Fuel;
end AOC.AOC_2019.Day01;
