--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into AOC.AOC_2019.Day01.Test_Data.

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
package body AOC.AOC_2019.Day01.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Calc_Fuel (Gnattest_T : in out Test);
   procedure Test_Calc_Fuel_9e45f9 (Gnattest_T : in out Test) renames Test_Calc_Fuel;
--  id:2.2/9e45f9acf0400023/Calc_Fuel/1/0/
   procedure Test_Calc_Fuel (Gnattest_T : in out Test) is
   --  aoc-aoc_2019-day01.ads:16:4:Calc_Fuel
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure A (M : Mass; F : Fuel) is
      begin
         Assert (Calc_Fuel (M) = F, "Fuel for" & M'Image & " <>" & F'Image);
      end A;

   begin

      A (12, 2);
      A (14, 2);
      A (1969, 654);
      A (100756, 33583);

--  begin read only
   end Test_Calc_Fuel;
--  end read only


--  begin read only
   procedure Test_Calc_Abhorrent_Fuel (Gnattest_T : in out Test);
   procedure Test_Calc_Abhorrent_Fuel_324660 (Gnattest_T : in out Test) renames Test_Calc_Abhorrent_Fuel;
--  id:2.2/324660d21572cbdb/Calc_Abhorrent_Fuel/1/0/
   procedure Test_Calc_Abhorrent_Fuel (Gnattest_T : in out Test) is
   --  aoc-aoc_2019-day01.ads:17:4:Calc_Abhorrent_Fuel
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure A (M : Mass; F : Fuel) is
      begin
         Assert (Calc_Abhorrent_Fuel (M) = F, "Abhorrent fuel for" & M'Image & " <>" & F'Image);
      end A;

   begin

      A (14, 2);
      A (1969, 966);
      A (100756, 50346);

--  begin read only
   end Test_Calc_Abhorrent_Fuel;
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
end AOC.AOC_2019.Day01.Test_Data.Tests;
