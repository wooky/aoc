with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body AOC.AOC_2019.Day08 is
   Width : Natural := 25;
   Height : Natural := 6;
   Image_Size : Natural := Width * Height;
   
   type Pixel is new Character range '0' .. '2';
   type Layer is array (0 .. Image_Size-1) of Pixel;
   
   Checksum : Natural;
   Result : Layer := (others => '2');
   
   procedure Init (D : Day_08; Root : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Root & "/input/2019/day08.txt");
   
      declare
         Image : String := Get_Line (File);
         Offset : Natural := Image'First;
         Zero_Count : Natural := Natural'Last;
      begin
         while Offset < Image'Last loop
            declare
               type Pixel_Count_Array is array (Pixel) of Natural;
               Pixel_Count : Pixel_Count_Array := (others => 0);
            begin
               for I in Layer'Range loop
                  declare
                     Current_Pixel : Pixel := Pixel (Image (Offset + I));
                  begin
                     Pixel_Count (Current_Pixel) := Pixel_Count (Current_Pixel) + 1;
                     if Result (I) = '2' then
                        Result (I) := Current_Pixel;
                     end if;
                  end;
               end loop;
            
               if Pixel_Count ('0') < Zero_Count then
                  Zero_Count := Pixel_Count ('0');
                  Checksum := Pixel_Count ('1') * Pixel_Count ('2');
               end if;
            end;
         
            Offset := Offset + Image_Size;
         end loop;
      end;
   
      Close (File);
   end Init;
   
   function Part_1 (D : Day_08) return String is
   begin
      return Checksum'Image;
   end Part_1;
   
   function Part_2 (D : Day_08) return String is
      Image : String (Result'First .. Result'Last + Height);
      Dest : Natural := Image'First;
   begin
      for I in Result'Range loop
         if I mod Width = 0 then
            Image (Dest) := Ada.Characters.Latin_1.LF;
            Dest := Dest + 1;
         end if;
         Image (Dest) := (if Result (I) = '1' then '#' else ' ');
         Dest := Dest + 1;
      end loop;
      return Image;
   end Part_2;
end AOC.AOC_2019.Day08;
