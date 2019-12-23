--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Intcode.Intcode_Instance_Test_Data is

   type Intcode_Instance_Access is access all GNATtest_Generated.GNATtest_Standard.Intcode.Intcode_Instance'Class;

--  begin read only
   type Test_Intcode_Instance is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Intcode_Instance_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Intcode_Instance);
   procedure Tear_Down (Gnattest_T : in out Test_Intcode_Instance);

end Intcode.Intcode_Instance_Test_Data;
