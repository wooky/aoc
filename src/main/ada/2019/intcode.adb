with Ada.Text_IO;
with GNAT.String_Split;

package body Intcode is
   function Compile (Filename : String) return Intcode_Compiler is
      use Ada.Text_IO;
      File : File_Type;
      
      use GNAT;
      Opcode_Set : String_Split.Slice_Set;
      
      Opcodes : Integer_Vectors.Vector;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Filename);
      
      declare
         Opcode_Contents : String := Get_Line (File);
      begin
         String_Split.Create (S => Opcode_Set,
                              From => Opcode_Contents,
                              Separators => ",",
                              Mode => String_Split.Multiple);
      end;
      
      for I in 1 .. String_Split.Slice_Count (Opcode_Set) loop
         Opcodes.Append (Integer'Value (String_Split.Slice (Opcode_Set, I)));
      end loop;
      
      Close (File);
      
      return (Opcodes => Opcodes);
   end Compile;
   
   function Instantiate (Compiler : Intcode_Compiler) return Intcode_Instance is
   begin
      return
        (Opcodes => Compiler.Opcodes,
         Inputs => Integer_Vectors.Empty_Vector,
         Last_Output => 0);
   end Instantiate;
   
   procedure Run (Instance : in out Intcode_Instance) is
      IP : Natural := 0;
   begin
      loop
         declare
            type Valid_Opcode is (Add, Multiply, Input, Output, JIT, JIF, LT, EQ, Halt);
            for Valid_Opcode use (1, 2, 3, 4, 5, 6, 7, 8, 99);
            Instruction : Natural := Instance.Opcodes (IP);
            Opcode : Valid_Opcode := Valid_Opcode'Enum_Val (Instruction mod 100);
            Immediate_1 : Boolean := (Instruction / 100) mod 2 = 1;
            Skip : Natural := 2;
         begin
            case Opcode is
            when Input =>
               Instance.Opcodes (Instance.Opcodes (IP+1)) := Instance.Inputs.First_Element;
               Instance.Inputs.Delete_First;
            when Output =>
               Instance.Last_Output := (if Immediate_1 then Instance.Opcodes (IP+1) else Instance.Opcodes (Instance.Opcodes (IP+1)));
            when Halt =>
               exit;
            when others =>
               Skip := 4;
               
               declare
                  Immediate_2 : Boolean := (Instruction / 1000) mod 2 = 1;
                  Val_1 : Integer := (if Immediate_1 then Instance.Opcodes (IP+1) else Instance.Opcodes (Instance.Opcodes (IP+1)));
                  Val_2 : Integer := (if Immediate_2 then Instance.Opcodes (IP+2) else Instance.Opcodes (Instance.Opcodes (IP+2)));
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
                     raise Constraint_Error with "Undefined opcode";
                  end case;
                  
                  if Store then
                     Instance.Opcodes (Instance.Opcodes (IP+3)) := Result;
                  end if;
               end;
            end case;
            
            IP := IP + Skip;
         end;
      end loop;
   end Run;
end Intcode;
