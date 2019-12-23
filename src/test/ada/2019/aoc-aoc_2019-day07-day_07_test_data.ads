--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AOC.Test_Data.Tests.Day.Day_Test_Data.Day_Tests;

with GNATtest_Generated;

package AOC.AOC_2019.Day07.Day_07_Test_Data is

--  begin read only
   type Test_Day_07 is new
     GNATtest_Generated.GNATtest_Standard.AOC.Test_Data.Tests.Day.Day_Test_Data.Day_Tests.Test_Day
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test_Day_07);
   procedure Tear_Down (Gnattest_T : in out Test_Day_07);

end AOC.AOC_2019.Day07.Day_07_Test_Data;
