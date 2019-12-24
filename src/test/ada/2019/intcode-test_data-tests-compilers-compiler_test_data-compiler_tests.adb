--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Intcode.Test_Data.Tests.Compilers.Compiler_Test_Data.

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
package body Intcode.Test_Data.Tests.Compilers.Compiler_Test_Data.Compiler_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Compile (Gnattest_T : in out Test_Compiler);
   procedure Test_Compile_9ba484 (Gnattest_T : in out Test_Compiler) renames Test_Compile;
--  id:2.2/9ba484c8d1d216af/Compile/1/0/
   procedure Test_Compile (Gnattest_T : in out Test_Compiler) is
   --  intcode.ads:34:7:Compile
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
   procedure Test_Instantiate (Gnattest_T : in out Test_Compiler);
   procedure Test_Instantiate_f7d1c7 (Gnattest_T : in out Test_Compiler) renames Test_Instantiate;
--  id:2.2/f7d1c70ad5abb651/Instantiate/1/0/
   procedure Test_Instantiate (Gnattest_T : in out Test_Compiler) is
   --  intcode.ads:35:7:Instantiate
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
   procedure Test_Compile_From_String (Gnattest_T : in out Test_Compiler);
   procedure Test_Compile_From_String_97394b (Gnattest_T : in out Test_Compiler) renames Test_Compile_From_String;
--  id:2.2/97394bbdab562724/Compile_From_String/1/0/
   procedure Test_Compile_From_String (Gnattest_T : in out Test_Compiler) is
   --  intcode.ads:41:7:Compile_From_String
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
end Intcode.Test_Data.Tests.Compilers.Compiler_Test_Data.Compiler_Tests;
