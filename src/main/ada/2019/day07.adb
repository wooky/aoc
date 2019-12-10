with Ada.Text_IO;
with Intcode;

procedure Day07 is
   use Ada.Text_IO;
   use Intcode;
   
   Compiler : Intcode_Compiler := Compile ("src/main/resources/2019/day07.txt");
   
   type Phases is array (0 .. 4) of Integer;
   Phase_Permutations : constant Phases := (0, 1, 2, 3, 4);
   
   function Get_Power (Phase_Permutation : Phases) return Integer is
      Power : Integer := 0;
   begin
      for Phase of Phase_Permutation loop
         declare
            Instance : Intcode_Instance := Instantiate (Compiler);
         begin
            Instance.Inputs.Append (Phase);
            Instance.Inputs.Append (Power);
            Instance.Run;
            Power := Instance.Outputs.Last_Element;
         end;
      end loop;
      
      return Power;
   end Get_Power;
   
   -- Uses Heap's algorithm
   function Get_Max_Power (K : Natural; A : in out Phases) return Integer is
      Max_Power : Integer := 0;
   begin
      if K = 1 then
         return Get_Power (A);
      end if;
      
      declare
         Max_Power : Integer := Get_Max_Power (K - 1, A);
         Temp : Integer;
      begin
         for I in 0 .. K-2 loop
            if K mod 2 = 0 then
               Temp := A (I);
               A (I) := A (K-1);
            else
               Temp := A (0);
               A (0) := A (K-1);
            end if;
            A (K-1) := Temp;
            
            Max_Power := Integer'Max (Max_Power, Get_Max_Power (K - 1, A));
         end loop;
         
         return Max_Power;
      end;
   end Get_Max_Power;
begin
   declare
      Phase_Permutation : Phases := Phase_Permutations;
   begin
      Put_Line (Get_Max_Power (Phase_Permutation'Length, Phase_Permutation)'Image);
   end;
end Day07;
