with Ada.Text_IO;
with Intcode;

procedure Day02 is
   use Ada.Text_IO;
   use Intcode;
   
   Compiler : Intcode_Compiler := Compile ("src/main/resources/2019/day02.txt");
   
   function Compute_Output (Noun, Verb : Integer) return Integer is
      Instance : Intcode_Instance := Instantiate (Compiler);
   begin
      Instance.Opcodes (1) := Noun;
      Instance.Opcodes (2) := Verb;
      
      Instance.Run;
      
      return Instance.Opcodes (0);
   end Compute_Output;
begin
   Put_Line (Integer'Image (Compute_Output (12, 2)));
   
   declare
      Result : Integer;
   begin
      Gravity_Assist: for Noun in 0 .. 99 loop
         for Verb in 0 .. 99 loop
            Result := Compute_Output (Noun, Verb);
            if Result = 19690720 then
               Put_Line (Integer'Image (100 * Noun + Verb));
               exit Gravity_Assist;
            end if;
         end loop;
      end loop Gravity_Assist;
   end;
end Day02;
