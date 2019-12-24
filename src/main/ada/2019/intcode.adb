with Ada.Text_IO;
with GNAT.String_Split;

package body Intcode is
   package body Compilers is
      procedure Compile (C : in out Compiler; Filename : String) is
         use Ada.Text_IO;
         File : File_Type;
      begin
         Open (File => File,
               Mode => In_File,
               Name => Filename);
      
         declare
            Opcode_Contents : String := Get_Line (File);
         begin
            C.Compile_From_String (Opcode_Contents);
         end;
         
         Close (File);
      end Compile;
   
      procedure Compile_From_String (C : in out Compiler; S : String) is
         use GNAT;
         Opcode_Set : String_Split.Slice_Set;
      begin
         String_Split.Create (S => Opcode_Set,
                              From => S,
                              Separators => ",",
                              Mode => String_Split.Multiple);
      
         for I in 1 .. String_Split.Slice_Count (Opcode_Set) loop
            C.Opcodes.Include (Index (I)-1, Element'Value (String_Split.Slice (Opcode_Set, I)));
         end loop;
      end Compile_From_String;
   
      function Instantiate (C : Compiler) return Instances.Instance is
      begin
         return
           (Opcodes => C.Opcodes, others => <>);
      end Instantiate;
   end Compilers;
   
   package body Instances is
      type Opcode_Mode is (Position, Immediate, Relative);
      
      function Get_Or_Set_Value_At_Address (I : in out Instance; Address : Index; Default_Value : Element := 0; Force_Set : Boolean := False) return Element is
      begin
         if not I.Opcodes.Contains (Address) then
            I.Opcodes.Include (Address, Default_Value);
            return Default_Value;
         elsif Force_Set then
            I.Opcodes (Address) := Default_Value;
            return Default_Value;
         end if;
         return I.Opcodes (Address);
      end Get_Or_Set_Value_At_Address;
   
      function Get_Value (I : in out Instance; Mode : Opcode_Mode := Immediate; Offset : Element := 0) return Element is
      begin
         return I.Get_Or_Set_Value_At_Address (case Mode is
                                                  when Position => I.Get_Or_Set_Value_At_Address (I.IP + Offset),
                                                  when Immediate => I.IP + Offset,
                                                  when Relative => I.IP + I.Relative_Base + Offset);
      end Get_Value;
   
      procedure Run (I : in out Instance) is
      begin
         if I.S = Halted then
            return;
         else
            I.S := Running;
         end if;
      
         loop
            declare
               type Valid_Opcode is (Add, Multiply, Input, Output, JIT, JIF, LT, EQ, ARB, Halt);
               for Valid_Opcode use (1, 2, 3, 4, 5, 6, 7, 8, 9, 99);
               Instruction : Element := I.Get_Value;
               Opcode : Valid_Opcode := Valid_Opcode'Enum_Val (Instruction mod 100);
               Mode_1 : Opcode_Mode := Opcode_Mode'Val ((Instruction / 100) mod Opcode_Mode'Size);
               Val_1 : Element := I.Get_Value (Mode_1, 1);
               Skip : Element := 2;
               Junk : Element;
            begin
               case Opcode is
               when Input =>
                  if I.Inputs.Is_Empty then
                     I.S := Need_Input;
                     return;
                  else
                     Junk := I.Get_Or_Set_Value_At_Address (I.Get_Value (Offset => 1), I.Inputs.First_Element, True);
                     I.Inputs.Delete_First;
                  end if;
               when Output =>
                  I.Outputs.Append (Val_1);
               when ARB =>
                  I.Relative_Base := I.Relative_Base + Val_1;
               when Halt =>
                  I.S := Halted;
                  return;
               when others =>
                  Skip := 4;
               
                  declare
                     Mode_2 : Opcode_Mode := Opcode_Mode'Val ((Instruction / 1000) mod Opcode_Mode'Size);
                     Val_2 : Element := I.Get_Value (Mode_2, 2);
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
                           I.IP := Val_2;
                           Skip := 0;
                        else
                           Skip := 3;
                        end if;
                     when JIF =>
                        Store := False;
                        if Val_1 = 0 then
                           I.IP := Val_2;
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
                  
                     Junk := I.Get_Or_Set_Value_At_Address (I.Get_Value (Offset => 3), Result, Store);
                  end;
               end case;
            
               I.IP := I.IP + Skip;
            end;
         end loop;
      end Run;
   end Instances;
end Intcode;
