with Ada.Text_IO;
with Ada.Strings.Fixed; -- remove me

procedure Day04 is
   use Ada.Text_IO;
   
   type Password is new String (1..6);
   Start : constant Password := "357253";
   Finish : constant Password := "892942";
   
   Combinations : Natural := 0;
   
   procedure Increment_Combinations (From, To, Amount : Natural; Prefix : String) is
   begin
      if Amount = 3 then
         declare
            Before : Natural := Combinations;
            Add : Natural := To - From;
            Subtract : Natural := 10 - To;
         begin
            Combinations := Combinations + (Add*(Add+1)*(Add+2) - Subtract*(Subtract+1)*(Subtract+2)) / 6;
            Put_Line (String (Prefix) & "XXX ->" & Natural'Image (Combinations - Before));
         end;
      else
         for I in From .. To-1 loop
            Increment_Combinations (I, 10, Amount-1, String (Prefix) & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Left));
         end loop;
      end if;
   end Increment_Combinations;
begin
   declare
      Start_Increasing_Digits : Natural := Start'First;
   begin
      while Start (Start_Increasing_Digits) <= Start (Start_Increasing_Digits + 1) loop
         Start_Increasing_Digits := Start_Increasing_Digits + 1;
      end loop;
      Increment_Combinations (Character'Pos (Start (Start_Increasing_Digits)) - 48, 10, Start'Length - Start_Increasing_Digits, String (Start (Start'First .. Start_Increasing_Digits)));
      for I in Start'First+1 .. Start_Increasing_Digits loop
         Increment_Combinations (Character'Pos (Start (I)) - 47, 10, Start'Length - I + Start'First, String (Start (Start'First .. I-1)));
      end loop;
   end;
   
   for I in Character'Pos (Start (Start'First))-47 .. Character'Pos (Finish (Finish'First))-49 loop
      Increment_Combinations (I, 10, Password'Length - 1, Ada.Strings.Fixed.Trim(I'Image, Ada.Strings.Left));
   end loop;
   
   declare
      Finish_Increasing_Digits : Natural := Finish'First;
   begin
      while Finish (Finish_Increasing_Digits) <= Finish (Finish_Increasing_Digits + 1) loop
         Increment_Combinations (Character'Pos (Finish (Finish_Increasing_Digits)) - 48, Character'Pos (Finish (Finish_Increasing_Digits+1)) - 48, Finish'Length - Finish_Increasing_Digits + Finish'First - 1, String (Finish (Finish'First .. Finish_Increasing_Digits)));
         Finish_Increasing_Digits := Finish_Increasing_Digits + 1;
      end loop;
   end;
   
   Put_Line (Combinations'Image);
end Day04;
