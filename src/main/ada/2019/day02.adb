with Ada.Containers.Vectors;
with Ada.Text_IO;
with GNAT.String_Split;

procedure Day02 is
   use Ada.Text_IO;
   File : File_Type;
   
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Integer);
   Opcodes_Copy : Integer_Vectors.Vector;
   
   function Compute_Output (Noun, Verb : Integer) return Integer is
      Opcodes : Integer_Vectors.Vector := Opcodes_Copy;
      I : Natural := 0;
      Destination : Natural;
      Result : Integer;
      Unknown_Opcode_Except : exception;
   begin
      Opcodes (1) := Noun;
      Opcodes (2) := Verb;
      
      loop
         case Opcodes (I) is
         when 1 =>
            Destination := Opcodes (I+3);
            Result := Opcodes (Opcodes (I+1)) + Opcodes (Opcodes (I+2));
         when 2 =>
            Destination := Opcodes (I+3);
            Result := Opcodes (Opcodes (I+1)) * Opcodes (Opcodes (I+2));
         when 99 =>
            exit;
         when others =>
            raise Unknown_Opcode_Except with "?????";
         end case;
         
         Opcodes (Destination) := Result;
         I := I + 4;
      end loop;
      
      return Opcodes (0);
   end Compute_Output;
begin
   Open (File => File,
         Mode => In_File,
         Name => "src/main/resources/2019/day02.txt");
   declare
      use GNAT;
      Opcode_Contents : String := Get_Line (File);
      Opcode_Set : String_Split.Slice_Set;
   begin
      String_Split.Create (S => Opcode_Set,
                           From => Opcode_Contents,
                           Separators => ",",
                           Mode => String_Split.Multiple);
      
      for I in 1 .. String_Split.Slice_Count (Opcode_Set) loop
         declare
            Opcode : String := String_Split.Slice (Opcode_Set, I);
         begin
            Opcodes_Copy.Append (Integer'Value (Opcode));
         end;
      end loop;
   end;
   Close (File);
   
   Put_Line (Integer'Image (Compute_Output (12, 2)));
   
   declare
      Done : Boolean := False;
      Result : Integer;
   begin
      for Noun in 0 .. 99 loop
         for Verb in 0 .. 99 loop
            Result := Compute_Output (Noun, Verb);
            if Result = 19690720 then
               Put_Line (Integer'Image (100 * Noun + Verb));
               Done := True;
               exit;
            end if;
         end loop;
         exit when Done;
      end loop;
   end;
end Day02;
