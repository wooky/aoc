with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

package Intcode is
   subtype Element is Long_Long_Integer;
   subtype Index is Element range 0 .. Element'Last;
   
   package Opcode_Vectors is new Ada.Containers.Ordered_Maps
     (Key_Type => Index,
      Element_Type => Element);
   
   package Instances is
      package Data_Vectors is new Ada.Containers.Vectors
        (Index_Type => Index,
         Element_Type => Element);
      
      type State is (Not_Started, Running, Need_Input, Halted);
      
      type Instance is tagged limited record
         Opcodes : Opcode_Vectors.Map;
         Inputs : Data_Vectors.Vector := Data_Vectors.Empty_Vector;
         Outputs : Data_Vectors.Vector := Data_Vectors.Empty_Vector;
         IP : Index := 0;
         S : State := Not_Started;
         Relative_Base : Element := 0;
      end record;
      
      procedure Run (I : in out Instance);
   end Instances;
   
   package Compilers is
      type Compiler is tagged limited private;
      
      procedure Compile (C : in out Compiler; Filename : String);
      function Instantiate (C : Compiler) return Instances.Instance;
   private
      type Compiler is tagged limited record
         Opcodes : Opcode_Vectors.Map := Opcode_Vectors.Empty_Map;
      end record;
      
      procedure Compile_From_String (C : in out Compiler; S : String);
   end Compilers;
end Intcode;
