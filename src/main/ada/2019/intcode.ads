with Ada.Containers.Vectors;

package Intcode is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Integer);
   
   type Intcode_Compiler is limited private;
   
   type Intcode_Instance is tagged limited record
      Opcodes : Integer_Vectors.Vector;
      Inputs : Integer_Vectors.Vector;
      Last_Output : Integer;
   end record;

   function Compile (Filename : String) return Intcode_Compiler;
   
   function Instantiate (Compiler : Intcode_Compiler) return Intcode_Instance;
   
   procedure Run (Instance : in out Intcode_Instance);
private
   type Intcode_Compiler is limited record
      Opcodes : Integer_Vectors.Vector;
   end record;
end Intcode;
