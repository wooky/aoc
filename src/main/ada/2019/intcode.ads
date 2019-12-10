with Ada.Containers.Vectors;

package Intcode is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Integer);
   use Integer_Vectors;
   
   type Intcode_Compiler is limited private;
   
   type Instance_State is (Not_Started, Running, Need_Input, Halted);
   
   type Intcode_Instance is tagged limited record
      Opcodes : Vector;
      Inputs : Vector := Empty_Vector;
      Outputs : Vector := Empty_Vector;
      IP : Integer := 0;
      State : Instance_State := Not_Started;
   end record;

   function Compile (Filename : String) return Intcode_Compiler;
   
   function Instantiate (Compiler : Intcode_Compiler) return Intcode_Instance;
   
   procedure Run (Instance : in out Intcode_Instance);
private
   type Intcode_Compiler is limited record
      Opcodes : Integer_Vectors.Vector;
   end record;
end Intcode;
