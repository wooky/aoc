with Interfaces.C; use Interfaces.C;

procedure Run (Year, Day : Positive)
  with
    Export => True,
    Convention => C,
    External_Name => "run_ada";
