--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Intcode.Test_Data.

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
package body Intcode.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Compile (Gnattest_T : in out Test);
   procedure Test_Compile_8fc19b (Gnattest_T : in out Test) renames Test_Compile;
--  id:2.2/8fc19b4c47be0d89/Compile/1/0/
   procedure Test_Compile (Gnattest_T : in out Test) is
   --  intcode.ads:30:4:Compile
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Compile;
--  end read only


--  begin read only
   procedure Test_Compile_From_String (Gnattest_T : in out Test);
   procedure Test_Compile_From_String_2808ba (Gnattest_T : in out Test) renames Test_Compile_From_String;
--  id:2.2/2808ba8ce1b051f3/Compile_From_String/1/0/
   procedure Test_Compile_From_String (Gnattest_T : in out Test) is
   --  intcode.ads:43:4:Compile_From_String
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Compile_From_String;
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
end Intcode.Test_Data.Tests;
