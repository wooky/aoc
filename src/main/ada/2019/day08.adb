with Ada.Text_IO;

procedure Day08 is
   use Ada.Text_IO;
   File : File_Type;
   
   Width : Natural := 25;
   Height : Natural := 6;
   Image_Size : Natural := Width * Height;
   
   type Pixel is new Character range '0' .. '2';
   type Layer is array (0 .. Image_Size-1) of Pixel;
   
   Checksum : Natural;
   Result : Layer := (others => '2');
begin
   Open (File => File,
         Mode => In_File,
         Name => "src/main/resources/2019/day08.txt");
   
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
   
   Put_Line (Checksum'Image);
   
   for I in Result'Range loop
      if I mod Width = 0 then
         Put_Line ("");
      end if;
      Put (if Result (I) = '1' then "#" else " ");
   end loop;
end Day08;
