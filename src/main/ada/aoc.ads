with Ada.Containers.Indefinite_Holders;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package AOC is
  pragma Preelaborate;

  type Solution is record
    S1 : chars_ptr;
    S2 : chars_ptr;
  end record
    with
      Convention => C;

  function New_Solution (S1, S2 : String) return Solution;

  type Rectangle is array(Positive range <>, Positive range <>) of Character;

  type Aoc_File is private;
  function New_File (Input : chars_ptr) return Aoc_File;
  function End_Of_File (File : Aoc_File) return Boolean;
  function Get_Line (File : Aoc_File) return String;
  procedure Reset (File : Aoc_File);
  function Read_Rectangle (File : Aoc_File) return Rectangle;

private

  package String_Holder is new Ada.Containers.Indefinite_Holders (String);
  type Aoc_File_Record is record
    Input: String_Holder.Holder;
    Pos : Positive := 1;
    Eof : Boolean := False;
  end record;
  type Aoc_File is access Aoc_File_Record;
end AOC;
