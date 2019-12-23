with Ada.Text_IO;

package body AOC.AOC_2019.Day01 is
   type Mass is mod 2 ** 32;
   subtype Fuel is Mass;

   Total_Fuel : Fuel := 0;
   Abhorrent_Total_Fuel : Fuel := 0;

   procedure Init (D : Day_01) is
      function Calc_Fuel (M : Mass) return Fuel is
         Result : Integer;
      begin
         Result := Integer (M) / 3 - 2;
         return Fuel (if Result < 0 then 0 else Result);
      end Calc_Fuel;

      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => "src/main/resources/2019/day01.txt");
      while not End_Of_File (File) loop
         declare
            Current_Mass : Mass := Mass (Integer'Value (Get_Line (File)));
            Current_Fuel : Fuel := Calc_Fuel (Current_Mass);
            Abhorrent_Current_Fuel : Fuel := Current_Fuel;
         begin
            Total_Fuel := Total_Fuel + Current_Fuel;
            while Abhorrent_Current_Fuel > 0 loop
               Abhorrent_Total_Fuel := Abhorrent_Total_Fuel + Abhorrent_Current_Fuel;
               Abhorrent_Current_Fuel := Calc_Fuel (Abhorrent_Current_Fuel);
            end loop;
         end;
      end loop;
      Close (File);
   end Init;

   function Part_1 (D : Day_01) return String is
   begin
      return Total_Fuel'Image;
   end Part_1;

   function Part_2 (D : Day_01) return String is
   begin
      return Abhorrent_Total_Fuel'Image;
   end Part_2;
end AOC.AOC_2019.Day01;
