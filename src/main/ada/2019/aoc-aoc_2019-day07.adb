with Intcode;

package body AOC.AOC_2019.Day07 is
   use Intcode;
   Compiler : Intcode_Compiler;
   
   type Phases is array (0 .. 4) of Element;
   
   type Power_Func is access function (Phase_Permutations : Phases) return Element;
   
   -- Uses Heap's algorithm
   function Get_Max_Power (K : Natural; A : in out Phases; F : Power_Func) return Element is
      Max_Power : Element := 0;
   begin
      if K = 1 then
         return F (A);
      end if;
      
      declare
         Max_Power : Element := Get_Max_Power (K - 1, A, F);
         Temp : Element;
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
            
            Max_Power := Element'Max (Max_Power, Get_Max_Power (K - 1, A, F));
         end loop;
         
         return Max_Power;
      end;
   end Get_Max_Power;
   
   procedure Init (D : Day_07; Root : String) is
   begin
      Compiler := Compile (Root & "/input/2019/day07.txt");
   end Init;
   
   function Part_1 (D : Day_07) return String is
      Phase_Permutation : Phases := (0, 1, 2, 3, 4);
      
      function Get_Power (Phase_Permutation : Phases) return Element is
         Power : Element := 0;
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
   begin
      return Get_Max_Power (Phase_Permutation'Length, Phase_Permutation, Get_Power'Unrestricted_Access)'Image;
   end Part_1;
   
   function Part_2 (D : Day_07) return String is
      Phase_Permutation : Phases := (5, 6, 7, 8, 9);
      
      function Get_Power (Phase_Permutation : Phases) return Element is
         Power : Element := 0;
         
         type Instance_Array is array (Phases'Range) of Intcode_Instance;
         Instances : Instance_Array := (others => Instantiate (Compiler));
         
         All_Halted : Boolean := False;
      begin
         for I in Phases'Range loop
            Instances (I).Inputs.Append (Phase_Permutation (I));
         end loop;
         Instances (Instances'First).Inputs.Append (0);
         
         while not All_Halted loop
            All_Halted := True;
            for I in Phases'Range loop
               Instances (I).Run;
               
               if I = Phases'Last then
                  if not Instances (I).Outputs.Is_Empty then
                     Instances (Phases'First).Inputs.Append (Instances (I).Outputs);
                     Power := Instances (I).Outputs.Last_Element;
                  end if;
               else
                  Instances (I+1).Inputs.Append (Instances (I).Outputs);
               end if;
               Instances (I).Outputs.Clear;
               
               if Instances (I).State /= Halted then
                  All_Halted := False;
               end if;
            end loop;
         end loop;
         
         return Power;
      end Get_Power;
   begin
      return Get_Max_Power (Phase_Permutation'Length, Phase_Permutation, Get_Power'Unrestricted_Access)'Image;
   end Part_2;
end AOC.AOC_2019.Day07;
