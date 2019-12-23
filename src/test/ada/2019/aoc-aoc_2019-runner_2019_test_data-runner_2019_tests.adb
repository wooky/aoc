--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into AOC.AOC_2019.Runner_2019_Test_Data.

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
package body AOC.AOC_2019.Runner_2019_Test_Data.Runner_2019_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

   R : Runner_2019;

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Day (Gnattest_T : in out Test_Runner_2019);
   procedure Test_Get_Day_75f64d (Gnattest_T : in out Test_Runner_2019) renames Test_Get_Day;
--  id:2.2/75f64d65cf2ee287/Get_Day/1/0/
   procedure Test_Get_Day (Gnattest_T : in out Test_Runner_2019) is
   --  aoc-aoc_2019.ads:4:4:Get_Day
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test_Day (Day_Number : Natural; Expected_Part_1, Expected_Part_2 : String) is
         D : Day.Access_Day := R.Get_Day (Day_Number);
      begin
         D.Init ("..");
         Assert (D.Part_1 = Expected_Part_1, "Day" & Day_Number'Image & " part 1");
         Assert (D.Part_2 = Expected_Part_2, "Day" & Day_Number'Image & " part 1");
      end Test_Day;

   begin

      Test_Day (1, Integer'Image (3434390), Integer'Image (5148724));
      Test_Day (2, Integer'Image (8017076), Integer'Image (3146));
      Test_Day (3, Integer'Image (651), Integer'Image (7534));
      Test_Day (5, Integer'Image (15426686), Integer'Image (11430197));
      Test_Day (6, Integer'Image (273985), Integer'Image (460));
      Test_Day (7, Integer'Image (13848), Integer'Image (12932154));
      --        Test_Day (8, Integer'Image (2250), ????);   TODO

--  begin read only
   end Test_Get_Day;
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
end AOC.AOC_2019.Runner_2019_Test_Data.Runner_2019_Tests;
