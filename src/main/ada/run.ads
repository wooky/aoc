with AOC; use AOC;
with Interfaces.C; use Interfaces.C;

function Run (Year, Day : Positive) return Solution
  with
    Export => True,
    Convention => C,
    External_Name => "run_ada";
