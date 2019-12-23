--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body AOC.AOC_2019.Day08.Day_08_Test_Data is

   Local_Day_08 : aliased GNATtest_Generated.GNATtest_Standard.AOC.AOC_2019.Day08.Day_08;
   procedure Set_Up (Gnattest_T : in out Test_Day_08) is
   begin
      GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Day.Day_Test_Data.Day_Tests.Set_Up
        (GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Day.Day_Test_Data.Day_Tests.Test_Day (Gnattest_T));
      Gnattest_T.Fixture := Local_Day_08'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Day_08) is
   begin
      GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Day.Day_Test_Data.Day_Tests.Tear_Down
        (GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Day.Day_Test_Data.Day_Tests.Test_Day (Gnattest_T));
   end Tear_Down;

end AOC.AOC_2019.Day08.Day_08_Test_Data;
