with Ada.Text_IO; use Ada.Text_IO;

procedure Day01 is
   type Mass is mod 2 ** 32;
   subtype Fuel is Mass;

   function Calc_Fuel (M : Mass) return Fuel is
      Result : Integer;
   begin
      Result := Integer (M) / 3 - 2;
      return Fuel (if Result < 0 then 0 else Result);
   end Calc_Fuel;

   File : File_Type;
   Total_Fuel : Fuel := 0;
   Abhorrent_Total_Fuel : Fuel := 0;
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

   Put_Line (Total_Fuel'Image);
   Put_Line (Abhorrent_Total_Fuel'Image);
end Day01;
