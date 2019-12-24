with Ada.Text_IO;

package body AOC.AOC_2019.Day01 is
   function Create return Day.Access_Day is
   begin
      return new Day_01' (others => <>);
   end Create;

   procedure Init (D : in out Day_01; Root : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Root & "/input/2019/day01.txt");
      while not End_Of_File (File) loop
         declare
            Current_Mass : Mass := Mass (Integer'Value (Get_Line (File)));
         begin
            D.Total_Fuel := D.Total_Fuel + Calc_Fuel (Current_Mass);
            D.Abhorrent_Total_Fuel := D.Abhorrent_Total_Fuel + Calc_Abhorrent_Fuel (Current_Mass);
         end;
      end loop;
      Close (File);
   end Init;

   function Part_1 (D : Day_01) return String is
   begin
      return D.Total_Fuel'Image;
   end Part_1;

   function Part_2 (D : Day_01) return String is
   begin
      return D.Abhorrent_Total_Fuel'Image;
   end Part_2;

   function Calc_Fuel (M : Mass) return Fuel is
      Result : Integer;
   begin
      Result := Integer (M) / 3 - 2;
      return Fuel (if Result < 0 then 0 else Result);
   end Calc_Fuel;

   function Calc_Abhorrent_Fuel (M : Mass) return Fuel is
      Abhorrent_Current_Fuel : Fuel := Calc_Fuel (M);
      Abhorrent_Total_Fuel : Fuel := 0;
   begin
      while Abhorrent_Current_Fuel > 0 loop
         Abhorrent_Total_Fuel := Abhorrent_Total_Fuel + Abhorrent_Current_Fuel;
         Abhorrent_Current_Fuel := Calc_Fuel (Abhorrent_Current_Fuel);
      end loop;
      return Abhorrent_Total_Fuel;
   end Calc_Abhorrent_Fuel;
end AOC.AOC_2019.Day01;
