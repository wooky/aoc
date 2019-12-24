with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Text_IO;
with GNAT.String_Split;

package body AOC.AOC_2019.Day06 is
   type Planet is new String (1..3);
   
   function Orbit_Maps_Hash (Key : Planet) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Key));
   end;
   
   package Planets is new Ada.Containers.Ordered_Sets
     (Element_Type => Planet);
   
   package Orbit_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type => Planet,
        Element_Type => Planets.Set,
        Hash => Orbit_Maps_Hash,
        Equivalent_Keys => "=",
        "=" => Planets."=");
   
   Orbits : Orbit_Maps.Map;
   
   package Orbit_Paths is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Planet);
   
   package Orbit_Processings is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Orbit_Paths.Vector,
      "=" => Orbit_Paths."=");
   
   Orbit_Processing : Orbit_Processings.Vector;
   
   Total_Orbits : Natural := 0;
   You_Path : Orbit_Paths.Vector;
   San_Path : Orbit_Paths.Vector;
   
   procedure Init (D : in out Day_06; Root : String) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => Root & "/input/2019/day06.txt");
      while not End_Of_File (File) loop
         declare
            use GNAT;
            Orbit_Line : String := Get_Line (File);
            Orbit_Set : String_Split.Slice_Set;
         begin
            String_Split.Create
              (S => Orbit_Set,
               From => Orbit_Line,
               Separators => ")",
               Mode => String_Split.Single);
         
            declare
               Key : Planet := Planet (String_Split.Slice (Orbit_Set, 1));
               Value : Planet := Planet (String_Split.Slice (Orbit_Set, 2));
            begin
               if Orbits.Contains (Key) then
                  Orbits (Key).Insert (Value);
               else
                  declare
                     Planets_Set : Planets.Set;
                  begin
                     Planets_Set.Insert (Value);
                     Orbits.Include (Key, Planets_Set);
                  end;
               end if;
            end;
         end;
      end loop;
   
      declare
         Com_Path : Orbit_Paths.Vector;
      begin
         Com_Path.Append ("COM");
         Orbit_Processing.Append (Com_Path);
      end;
   
      while not Orbit_Processing.Is_Empty loop
         declare
            Path : Orbit_Paths.Vector := Orbit_Processing.First_Element;
            Orbiter : Planet := Path.Last_Element;
         begin
            Total_Orbits := Total_Orbits + Natural (Path.Length) - 1;
            Orbit_Processing.Delete_First;
         
            if Orbits.Contains (Orbiter) then
               for P of Orbits (Orbiter) loop
                  declare
                     New_Path : Orbit_Paths.Vector := Path;
                  begin
                     New_Path.Append (P);
                     Orbit_Processing.Append (New_Path);
                  end;
               end loop;
            elsif Orbiter = "YOU" then
               You_Path := Path;
            elsif Orbiter = "SAN" then
               San_Path := Path;
            end if;
         end;
      end loop;
   end Init;
   
   function Part_1 (D : Day_06) return String is
   begin
      return Total_Orbits'Image;
   end Part_1;
   
   function Part_2 (D : Day_06) return String is
      Common_Orbit_Count : Natural := Natural'First;
   begin      
      while You_Path (Common_Orbit_Count) = San_Path (Common_Orbit_Count) loop
         Common_Orbit_Count := Common_Orbit_Count + 1;
      end loop;
      
      return Natural'Image (Natural (You_Path.Length) + Natural (San_Path.Length) - (Common_Orbit_Count + 1)*2);
   end Part_2;
end AOC.AOC_2019.Day06;
