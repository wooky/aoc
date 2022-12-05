package body AOC is
  function New_Solution (S1, S2 : String) return Solution is
  begin
    return (S1 => New_String (S1), S2 => New_String (S2));
  end New_Solution;
end AOC;