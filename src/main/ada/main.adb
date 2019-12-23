with Ada.Command_Line;
with Ada.Text_IO;
with AOC.AOC_2019;

procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   Year : Natural := Natural'Value (Argument (1));
   Day_Number : Natural := Natural'Value (Argument (2));
   Runner : AOC.Runner.Access_Runner := (case Year is
                                            when 2019 => new AOC.AOC_2019.Runner_2019,
                                            when others => raise Constraint_Error with "Invalid year");
   Day : AOC.Day.Access_Day := Runner.Get_Day (Day_Number);
begin
   if Day in null then
      raise Constraint_Error with "Invalid day";
   end if;
   Day.Init (".");
   Put_Line (Day.Part_1);
   Put_Line (Day.Part_2);
end Main;
