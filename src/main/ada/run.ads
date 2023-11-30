with AOC; use AOC;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

function Run (File_C : chars_ptr; Year, Day : Positive) return Solution
  with
    Export => True,
    Convention => C,
    External_Name => "run";
