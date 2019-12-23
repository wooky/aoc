with Intcode;

package body AOC.AOC_2019.Day05 is
   use Intcode;
   Compiler : Intcode_Compiler;
   
   function Run_Diagnostics (Input : Element) return Element is
      Instance : Intcode_Instance := Instantiate (Compiler);
   begin
      Instance.Inputs.Append (Input);
      Instance.Run;
      return Instance.Outputs.Last_Element;
   end Run_Diagnostics;
   
   procedure Init (D : Day_05; Root : String) is
   begin
      Compiler := Compile (Root & "/input/2019/day05.txt");
   end Init;
   
   function Part_1 (D : Day_05) return String is
   begin
      return Run_Diagnostics (1)'Image;
   end Part_1;
   
   function Part_2 (D : Day_05) return String is
   begin
      return Run_Diagnostics (5)'Image;
   end Part_2;
end AOC.AOC_2019.Day05;
