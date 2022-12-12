with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with AOC; use AOC;

function Day07 return Solution is
  type Content_Type is (File, Directory);
  type Content (Kind : Content_Type := File) is record
    case Kind is
      when File =>
        Size : Natural;
      when Directory =>
        Full_Path: Unbounded_String;
    end case;
  end record;

  package Directory_Contents_Vector is new Ada.Containers.Vectors (
    Index_Type => Natural,
    Element_Type => Content
  );
  subtype Directory_Contents is Directory_Contents_Vector.Vector;

  type Directory_Information is record
    Size : Natural := 0;
    Contents : Directory_Contents;
  end record;

  -- To satisfy Directory_Listing_Map."="
  function "=" (X, Y : Directory_Contents) return Boolean is
  begin
    return False;
  end "=";

  package Directory_Listing_Map is new Ada.Containers.Indefinite_Hashed_Maps (
    Key_Type => String,
    Element_Type => Directory_Information,
    Hash => Ada.Strings.Hash,
    Equivalent_Keys => "="
  );
  subtype Directory_Listing is Directory_Listing_Map.Map;

  function Change_Directory (Current_Directory : Unbounded_String ; Where_To : String) return String is
  begin
    if Where_To = "/" then
      return "/";
    elsif Where_To = ".." then
      declare
        Idx : Positive := Index (Current_Directory, "/", Length (Current_Directory) - 1, Ada.Strings.Backward);
      begin
        return Slice (Current_Directory, 1, Idx);
      end;
    else
      return To_String (Current_Directory & Where_To & "/");
    end if;
  end Change_Directory;

  procedure Fill_Directory_Size (
    Directory_Listings : in out Directory_Listing;
    Current_Directory : String
  ) is
    Total_Size : Natural := 0;
  begin
    for Contents of Directory_Listings (Current_Directory).Contents loop
      case Contents.Kind is
        when File => Total_Size := Total_Size + Contents.Size;
        when Directory =>
          Fill_Directory_Size (Directory_Listings, To_String (Contents.Full_Path));
          Total_Size := Total_Size + Directory_Listings (To_String (Contents.Full_Path)).Size;
      end case;
    end loop;
    Directory_Listings.Reference (Current_Directory).Size := Total_Size;
  end Fill_Directory_Size;

  Directory_Listings : Directory_Listing;
  Current_Directory : Unbounded_String;
begin
  declare
    F : File_Type;
  begin
    Open (F, In_File, "input/2022/day07.txt");
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
        Space1 : Positive := Index (Line, " ");
        Token1 : String := Line (Line'First .. Space1 - 1);
        Token2 : String := Line (Space1 + 1 .. Line'Last);
        Space2 : Natural := Index (Line, " ", Space1 + 1);
        Current_Directory_String : String := To_String (Current_Directory);
      begin
        if Token1 = "$" then
          if Space2 /= 0 then
            Current_Directory := To_Unbounded_String (Change_Directory (Current_Directory, Line (Space2 + 1 .. Line'Last)));
          end if;
        else
          if not Directory_Listings.Contains (Current_Directory_String) then
            Directory_Listings.Insert (Current_Directory_String, (
              Size => 0,
              Contents => Directory_Contents_Vector.Empty_Vector
            ));
          end if;
          if Token1 = "dir" then
            Directory_Listings (Current_Directory_String).Contents.Append ((
              Kind => Directory,
              Full_Path => To_Unbounded_String (Change_Directory (Current_Directory, Token2))
            ));
          else
            Directory_Listings (Current_Directory_String).Contents.Append ((
              Kind => File,
              Size => Natural'Value (Token1)
            ));
          end if;
        end if;
      end;
    end loop;
    Close (F);
  end;
  
  Fill_Directory_Size (Directory_Listings, "/");
  declare
    Space_Required : Positive := 30000000 - 70000000 + Directory_Listings ("/").Size;
    Filtered_Size : Natural := 0;
    Smallest_Deleted_Size : Positive := Positive'Last;
  begin
    for Info of Directory_Listings loop
      if Info.Size <= 100000 then
        Filtered_Size := Filtered_Size + Info.Size;
      end if;
      if Info.Size in Space_Required .. Smallest_Deleted_Size - 1 then
        Smallest_Deleted_Size := Info.Size;
      end if;
    end loop;
    return New_Solution (S1 => Filtered_Size'Image, S2 => Smallest_Deleted_Size'Image);
  end;
end Day07;
