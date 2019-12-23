--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Intcode.Intcode_Instance_Test_Data is

   Local_Intcode_Instance : aliased GNATtest_Generated.GNATtest_Standard.Intcode.Intcode_Instance;
   procedure Set_Up (Gnattest_T : in out Test_Intcode_Instance) is
   begin
      Gnattest_T.Fixture := Local_Intcode_Instance'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_Intcode_Instance) is
   begin
      null;
   end Tear_Down;

end Intcode.Intcode_Instance_Test_Data;
