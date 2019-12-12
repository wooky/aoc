with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

package Intcode is
   subtype Element is Long_Long_Integer;
   subtype Index is Element range 0 .. Element'Last;
   
   package Opcode_Vectors is new Ada.Containers.Ordered_Maps
     (Key_Type => Index,
      Element_Type => Element);
   
   package Data_Vectors is new Ada.Containers.Vectors
     (Index_Type => Index,
      Element_Type => Element);
   
   type Intcode_Compiler is limited private;
   
   type Instance_State is (Not_Started, Running, Need_Input, Halted);
   
   type Intcode_Instance is tagged limited record
      Opcodes : Opcode_Vectors.Map;
      Inputs : Data_Vectors.Vector := Data_Vectors.Empty_Vector;
      Outputs : Data_Vectors.Vector := Data_Vectors.Empty_Vector;
      IP : Index := 0;
      State : Instance_State := Not_Started;
      Relative_Base : Element := 0;
   end record;

   function Compile (Filename : String) return Intcode_Compiler;
   
   function Instantiate (Compiler : Intcode_Compiler) return Intcode_Instance;
   
   procedure Run (Instance : in out Intcode_Instance);
private
   type Intcode_Compiler is limited record
      Opcodes : Opcode_Vectors.Map;
   end record;
   
   type Opcode_Mode is (Position, Immediate, Relative);
end Intcode;
