--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body AOC.AOC_2019.Runner_2019_Test_Data is

   Local_Runner_2019 : aliased GNATtest_Generated.GNATtest_Standard.AOC.AOC_2019.Runner_2019;
   procedure Set_Up (Gnattest_T : in out Test_Runner_2019) is
   begin
      GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Runner.Runner_Test_Data.Runner_Tests.Set_Up
        (GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Runner.Runner_Test_Data.Runner_Tests.Test_Runner (Gnattest_T));
      Gnattest_T.Fixture := Local_Runner_2019'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Runner_2019) is
   begin
      GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Runner.Runner_Test_Data.Runner_Tests.Tear_Down
        (GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Runner.Runner_Test_Data.Runner_Tests.Test_Runner (Gnattest_T));
   end Tear_Down;

end AOC.AOC_2019.Runner_2019_Test_Data;
