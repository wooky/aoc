with Ada.Containers.Vectors;
with Ada.Text_IO;
with GNAT.String_Split;

package body AOC.AOC_2019.Day03 is
   type Wire_Segment is record
      Is_Horizontal: Boolean;
      Distance_Travelled : Integer;
      Time_Start : Integer;
      Stable, P1, P2 : Integer;
   end record;
   package Wire_Segment_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Wire_Segment);
   
   First_Wire_Segments : Wire_Segment_Vectors.Vector;
   
   Shortest_Distance : Integer := Integer'Last;
   Fastest_Distance : Integer := Integer'Last;
   
   function Create_Wire_Segments (Wire : String) return Wire_Segment_Vectors.Vector is
      use GNAT;
      Wire_Subs : String_Split.Slice_Set;
      
      Distance_Travelled : Integer := 0;
      X, Y : Integer := 0;
      X_Next, Y_Next : Integer;
      
      Wire_Segments : Wire_Segment_Vectors.Vector;
   begin
      String_Split.Create (S => Wire_Subs,
                           From => Wire,
                           Separators => ",");
      
      for I in 1 .. String_Split.Slice_Count (Wire_Subs) loop
         declare
            Wire_Sub : String := String_Split.Slice (Wire_Subs, I);
            Direction : Character := Wire_Sub (Wire_Sub'First);
            Offset : Integer := Integer'Value (Wire_Sub (Wire_Sub'First+1 .. Wire_Sub'Last));
            Segment : Wire_Segment;
         begin
            case Direction is
               when 'U' =>
                  Y_Next := Y + Offset;
                  Segment := (False, Distance_Travelled, Y, X, Y, Y_Next);
                  Y := Y_Next;
               when 'D' =>
                  Y_Next := Y - Offset;
                  Segment := (False, Distance_Travelled, Y, X, Y_Next, Y);
                  Y := Y_Next;
               when 'L' =>
                  X_Next := X - Offset;
                  Segment := (True, Distance_Travelled, X, Y, X_Next, X);
                  X := X_Next;
               when 'R' =>
                  X_Next := X + Offset;
                  Segment := (True, Distance_Travelled, X, Y, X, X_Next);
                  X := X_Next;
               when others =>
                  raise Constraint_Error;
            end case;
            
            Wire_Segments.Append (Segment);
            Distance_Travelled := Distance_Travelled + Offset;
         end;
      end loop;
      
      return Wire_Segments;
   end Create_Wire_Segments;
   
   procedure Init (D : Day_03) is
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File => File,
            Mode => In_File,
            Name => "src/main/resources/2019/day03.txt");
   
      First_Wire_Segments := Create_Wire_Segments (Get_Line (File));
      for Second_Wire_Segment of Create_Wire_Segments (Get_Line (File)) loop
         for First_Wire_Segment of First_Wire_Segments loop
            if Second_Wire_Segment.Is_Horizontal /= First_Wire_Segment.Is_Horizontal and not (Second_Wire_Segment.Stable = 0 and First_Wire_Segment.Stable = 0) and Second_Wire_Segment.Stable in First_Wire_Segment.P1..First_Wire_Segment.P2 and First_Wire_Segment.Stable in Second_Wire_Segment.P1..Second_Wire_Segment.P2 then
               Shortest_Distance := Integer'Min (Shortest_Distance, abs Second_Wire_Segment.Stable + abs First_Wire_Segment.Stable);
               Fastest_Distance := Integer'Min
                 (Fastest_Distance,
                  Second_Wire_Segment.Distance_Travelled + First_Wire_Segment.Distance_Travelled
                  + abs (Second_Wire_Segment.Time_Start - First_Wire_Segment.Stable)
                  + abs (First_Wire_Segment.Time_Start - Second_Wire_Segment.Stable));
            end if;
         end loop;
      end loop;
   
      Close (File);
   end Init;
   
   function Part_1 (D : Day_03) return String is
   begin
      return Shortest_Distance'Image;
   end Part_1;
   
   function Part_2 (D : Day_03) return String is
   begin
      return Fastest_Distance'Image;
   end Part_2;
end AOC.AOC_2019.Day03;
