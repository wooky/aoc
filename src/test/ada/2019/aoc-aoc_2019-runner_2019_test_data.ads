--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AOC.Test_Data.Tests.Runner.Runner_Test_Data.Runner_Tests;

with GNATtest_Generated;

package AOC.AOC_2019.Runner_2019_Test_Data is

--  begin read only
   type Test_Runner_2019 is new
     GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Runner.Runner_Test_Data.Runner_Tests.Test_Runner
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test_Runner_2019);
   procedure Tear_Down (Gnattest_T : in out Test_Runner_2019);

end AOC.AOC_2019.Runner_2019_Test_Data;
