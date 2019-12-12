with Ada.Text_IO;
with GNAT.String_Split;

package body Intcode is
   function Compile (Filename : String) return Intcode_Compiler is
      use Ada.Text_IO;
      File : File_Type;
      
      use GNAT;
      Opcode_Set : String_Split.Slice_Set;
      
      Opcodes : Opcode_Vectors.Map;
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
         Opcodes.Include (Index (I)-1, Element'Value (String_Split.Slice (Opcode_Set, I)));
      end loop;
      
      Close (File);
      
      return (Opcodes => Opcodes);
   end Compile;
   
   function Instantiate (Compiler : Intcode_Compiler) return Intcode_Instance is
   begin
      return
        (Opcodes => Compiler.Opcodes, others => <>);
   end Instantiate;
   
   function Get_Or_Set_Value_At_Address (Instance : in out Intcode_Instance; Address : Index; Default_Value : Element := 0; Force_Set : Boolean := False) return Element is
   begin
      if not Instance.Opcodes.Contains (Address) then
         Instance.Opcodes.Include (Address, Default_Value);
         return Default_Value;
      elsif Force_Set then
         Instance.Opcodes (Address) := Default_Value;
         return Default_Value;
      end if;
      return Instance.Opcodes (Address);
   end Get_Or_Set_Value_At_Address;
   
   function Get_Value (Instance : in out Intcode_Instance; Mode : Opcode_Mode := Immediate; Offset : Element := 0) return Element is
   begin
      return Instance.Get_Or_Set_Value_At_Address (case Mode is
                                                      when Position => Instance.Get_Or_Set_Value_At_Address (Instance.IP + Offset),
                                                      when Immediate => Instance.IP + Offset,
                                                      when Relative => Instance.IP + Instance.Relative_Base + Offset);
   end Get_Value;
   
   procedure Run (Instance : in out Intcode_Instance) is
   begin
      if Instance.State = Halted then
         return;
      else
         Instance.State := Running;
      end if;
      
      loop
         declare
            type Valid_Opcode is (Add, Multiply, Input, Output, JIT, JIF, LT, EQ, ARB, Halt);
            for Valid_Opcode use (1, 2, 3, 4, 5, 6, 7, 8, 9, 99);
            Instruction : Element := Instance.Get_Value;
            Opcode : Valid_Opcode := Valid_Opcode'Enum_Val (Instruction mod 100);
            Mode_1 : Opcode_Mode := Opcode_Mode'Val ((Instruction / 100) mod Opcode_Mode'Size);
            Val_1 : Element := Instance.Get_Value (Mode_1, 1);
            Skip : Element := 2;
            Junk : Element;
         begin
            case Opcode is
            when Input =>
               if Instance.Inputs.Is_Empty then
                  Instance.State := Need_Input;
                  return;
               else
                  Junk := Instance.Get_Or_Set_Value_At_Address (Instance.Get_Value (Offset => 1), Instance.Inputs.First_Element, True);
                  Instance.Inputs.Delete_First;
               end if;
            when Output =>
               Instance.Outputs.Append (Val_1);
            when ARB =>
               Instance.Relative_Base := Instance.Relative_Base + Val_1;
            when Halt =>
               Instance.State := Halted;
               return;
            when others =>
               Skip := 4;
               
               declare
                  Mode_2 : Opcode_Mode := Opcode_Mode'Val ((Instruction / 1000) mod Opcode_Mode'Size);
                  Val_2 : Element := Instance.Get_Value (Mode_2, 2);
                  Result : Element;
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
                  
                  Junk := Instance.Get_Or_Set_Value_At_Address (Instance.Get_Value (Offset => 3), Result, Store);
               end;
            end case;
            
            Instance.IP := Instance.IP + Skip;
         end;
      end loop;
   end Run;
end Intcode;
