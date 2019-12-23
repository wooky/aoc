package AOC is
   package Day is
      type Day is abstract tagged null record;
      type Access_Day is access Day'Class;

      procedure Init (D : Day) is abstract;
      function Part_1 (D : Day) return String is abstract;
      function Part_2 (D : Day) return String is abstract;
   end Day;

   package Runner is
      type Runner is abstract tagged null record;
      type Access_Runner is access Runner'Class;

      function Get_Day (R : Runner; Day_Number : Natural) return Day.Access_Day is abstract;
   end Runner;
end AOC;
