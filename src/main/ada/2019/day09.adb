with Ada.Text_IO;
with Intcode;

procedure Day09 is
   use Ada.Text_IO;
   use Intcode;
   
   Compiler : Intcode_Compiler := Compile ("src/main/resources/2019/day09.txt");
begin
   declare
      Instance : Intcode_Instance := Instantiate (Compiler);
   begin
      Instance.Inputs.Append (1);
      Instance.Run;
      pragma Assert (Instance.State = Halted);
      pragma Assert (Integer (Instance.Outputs.Length) = 1);
      Put_Line (Instance.Outputs.Last_Element'Image);
   end;
end Day09;
