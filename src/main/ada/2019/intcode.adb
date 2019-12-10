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
        (Opcodes => Compiler.Opcodes, others => <>);
   end Instantiate;
   
   function Get_Opcode (Instance : Intcode_Instance; Immediate : Boolean := True; Offset : Integer := 0) return Integer is
   begin
      return (if Immediate then Instance.Opcodes (Instance.IP+Offset) else Instance.Opcodes (Instance.Opcodes (Instance.IP+Offset)));
   end Get_Opcode;
   
   procedure Run (Instance : in out Intcode_Instance) is
   begin
      if Instance.State = Halted then
         return;
      else
         Instance.State := Running;
      end if;
      
      loop
         declare
            type Valid_Opcode is (Add, Multiply, Input, Output, JIT, JIF, LT, EQ, Halt);
            for Valid_Opcode use (1, 2, 3, 4, 5, 6, 7, 8, 99);
            Instruction : Natural := Instance.Get_Opcode;
            Opcode : Valid_Opcode := Valid_Opcode'Enum_Val (Instruction mod 100);
            Immediate_1 : Boolean := (Instruction / 100) mod 2 = 1;
            Skip : Natural := 2;
         begin
            case Opcode is
            when Input =>
               if Instance.Inputs.Is_Empty then
                  Instance.State := Need_Input;
                  return;
               else
                  Instance.Opcodes (Instance.Opcodes (Instance.IP+1)) := Instance.Inputs.First_Element;
                  Instance.Inputs.Delete_First;
               end if;
            when Output =>
               Instance.Outputs.Append (Instance.Get_Opcode (Immediate_1, 1));
            when Halt =>
               Instance.State := Halted;
               return;
            when others =>
               Skip := 4;
               
               declare
                  Immediate_2 : Boolean := (Instruction / 1000) mod 2 = 1;
                  Val_1 : Integer := Instance.Get_Opcode (Immediate_1, 1);
                  Val_2 : Integer := Instance.Get_Opcode (Immediate_2, 2);
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
                        Instance.IP := Val_2;
                        Skip := 0;
                     else
                        Skip := 3;
                     end if;
                  when JIF =>
                     Store := False;
                     if Val_1 = 0 then
                        Instance.IP := Val_2;
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
                     Instance.Opcodes (Instance.Opcodes (Instance.IP+3)) := Result;
                  end if;
               end;
            end case;
            
            Instance.IP := Instance.IP + Skip;
         end;
      end loop;
   end Run;
end Intcode;
