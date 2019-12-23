with Intcode;

package body AOC.AOC_2019.Day02 is
   use Intcode;
   Compiler : Intcode_Compiler;
   
   function Compute_Output (Noun, Verb : Integer) return Element is
      Instance : Intcode_Instance := Instantiate (Compiler);
   begin
      Instance.Opcodes (1) := Element (Noun);
      Instance.Opcodes (2) := Element (Verb);
      
      Instance.Run;
      
      return Instance.Opcodes (0);
   end Compute_Output;
   
   procedure Init (D : Day_02; Root : String) is
   begin
      Compiler := Compile (Root & "/input/2019/day02.txt");
   end Init;
   
   function Part_1 (D : Day_02) return String is
   begin
      return Compute_Output (12, 2)'Image;
   end Part_1;
   
   function Part_2 (D : Day_02) return String is
      Result : Element;
   begin
      for Noun in 0 .. 99 loop
         for Verb in 0 .. 99 loop
            Result := Compute_Output (Noun, Verb);
            if Result = 19690720 then
               return Integer'Image (100 * Noun + Verb);
            end if;
         end loop;
      end loop;
      
      raise Program_Error with "Output was not found!";
   end Part_2;
end AOC.AOC_2019.Day02;
