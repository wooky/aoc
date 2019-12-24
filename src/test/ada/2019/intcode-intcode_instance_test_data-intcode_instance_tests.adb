--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Intcode.Intcode_Instance_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Intcode.Intcode_Instance_Test_Data.Intcode_Instance_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Instantiate (Gnattest_T : in out Test_Intcode_Instance);
   procedure Test_Instantiate_767b56 (Gnattest_T : in out Test_Intcode_Instance) renames Test_Instantiate;
--  id:2.2/767b561ac18aae82/Instantiate/1/0/
   procedure Test_Instantiate (Gnattest_T : in out Test_Intcode_Instance) is
   --  intcode.ads:32:4:Instantiate
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Instantiate;
--  end read only


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test_Intcode_Instance);
   procedure Test_Run_d5af6c (Gnattest_T : in out Test_Intcode_Instance) renames Test_Run;
--  id:2.2/d5af6cecb8b3b4cf/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test_Intcode_Instance) is
   --  intcode.ads:34:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test_Day_2 (P : String; I : Index; E : Element) is
         Compiler : Intcode_Compiler := Compile_From_String (P);
         Instance : Intcode_Instance := Instantiate (Compiler);
      begin
         Instance.Run;
         Assert (Instance.Opcodes (I) = E, "Expected" & E'Image & " at index" & I'Image & " but was" & Element'Image (Instance.Opcodes (I)));
      end Test_Day_2;

      subtype SEN is Index range 7 .. 9;
      type SEN_Expectation is array (SEN) of Element;
      procedure Test_Day_5 (P : String; Expectation : SEN_Expectation) is
         Compiler : Intcode_Compiler := Compile_From_String (P);
      begin
         for I in SEN loop
            declare
               Instance : Intcode_Instance := Instantiate (Compiler);
            begin
               Instance.Inputs.Append (I);
               Instance.Run;
               Assert (Natural (Instance.Outputs.Length) = 1, "Expected one outputs but there were" & Instance.Outputs.Length'Image);
               Assert (Instance.Outputs.First_Element = Expectation (I), "Expected output to be" & Expectation (I)'Image & " but was" & Instance.Outputs.First_Element'Image);
            end;
         end loop;
      end Test_Day_5;

   begin

      Test_Day_2 ("1,9,10,3,2,3,11,0,99,30,40,50", 0, 3500);
      Test_Day_2 ("1,0,0,0,99", 0, 2);
      Test_Day_2 ("2,3,0,3,99", 3, 6);
      Test_Day_2 ("2,4,4,5,99,0", 5, 9801);
      Test_Day_2 ("1,1,1,4,99,5,6,0,99", 0, 30);
      Test_Day_2 ("1,1,1,4,99,5,6,0,99", 4, 2);

      Test_Day_5 ("3,9,8,9,10,9,4,9,99,-1,8", (0, 1, 0));
      Test_Day_5 ("3,9,7,9,10,9,4,9,99,-1,8", (1, 0, 0));
      Test_Day_5 ("3,3,1108,-1,8,3,4,3,99", (0, 1, 0));
      Test_Day_5 ("3,3,1107,-1,8,3,4,3,99", (1, 0, 0));
      Test_Day_5 ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," &
                    "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," &
                    "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99", (999, 1000, 1001));

--  begin read only
   end Test_Run;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Intcode.Intcode_Instance_Test_Data.Intcode_Instance_Tests;
