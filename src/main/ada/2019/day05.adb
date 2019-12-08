with Ada.Containers.Vectors;
with Ada.Text_IO;
with GNAT.String_Split;

procedure Day05 is
   use Ada.Text_IO;
   File : File_Type;
   
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Integer);
   Opcodes_Copy : Integer_Vectors.Vector;
   
   function Run_Diagnostics (The_Input : Integer) return Integer is
      Opcodes : Integer_Vectors.Vector := Opcodes_Copy;
      Last_Output : Integer;
      IP : Natural := 0;
   begin
      loop
         declare
            type Valid_Opcode is (Add, Multiply, Input, Output, JIT, JIF, LT, EQ, Halt);
            for Valid_Opcode use (1, 2, 3, 4, 5, 6, 7, 8, 99);
            Instruction : Natural := Opcodes (IP);
            Opcode : Valid_Opcode := Valid_Opcode'Enum_Val (Instruction mod 100);
            Immediate_1 : Boolean := (Instruction / 100) mod 2 = 1;
            Skip : Natural := 2;
         begin
            case Opcode is
            when Input =>
               Opcodes (Opcodes (IP+1)) := The_Input;
            when Output =>
               Last_Output := (if Immediate_1 then Opcodes (IP+1) else Opcodes (Opcodes (IP+1)));
            when Halt =>
               exit;
            when others =>
               Skip := 4;
               
               declare
                  Immediate_2 : Boolean := (Instruction / 1000) mod 2 = 1;
                  Val_1 : Integer := (if Immediate_1 then Opcodes (IP+1) else Opcodes (Opcodes (IP+1)));
                  Val_2 : Integer := (if Immediate_2 then Opcodes (IP+2) else Opcodes (Opcodes (IP+2)));
                  Result : Integer;
                  Store : Boolean := True;
               begin
                  case Opcode is
                  when Add =>
                     Result := Val_1 + Val_2;
                  when Multiply =>
                     Result := Val_1 * Val_2;
                  when JIT =>
                     Store := False;
                     if Val_1 /= 0 then
                        IP := Val_2;
                        Skip := 0;
                     else
                        Skip := 3;
                     end if;
                  when JIF =>
                     Store := False;
                     if Val_1 = 0 then
                        IP := Val_2;
                        Skip := 0;
                     else
                        Skip := 3;
                     end if;
                  when LT =>
                     Result := (if Val_1 < Val_2 then 1 else 0);
                  when EQ =>
                     Result := (if Val_1 = Val_2 then 1 else 0);
                  when others =>
                     Put_Line ("wtf");
                  end case;
                  
                  if Store then
                     Opcodes (Opcodes (IP+3)) := Result;
                  end if;
               end;
            end case;
            
            IP := IP + Skip;
         end;
      end loop;
      
      return Last_Output;
   end Run_Diagnostics;
begin
   Open (File => File,
         Mode => In_File,
         Name => "src/main/resources/2019/day05.txt");
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
   
   Put_Line (Run_Diagnostics (1)'Image);
   Put_Line (Run_Diagnostics (5)'Image);
end Day05;
