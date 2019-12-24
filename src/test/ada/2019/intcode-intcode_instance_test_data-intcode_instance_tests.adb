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

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

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
