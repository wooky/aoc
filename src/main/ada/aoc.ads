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
end AOC;
