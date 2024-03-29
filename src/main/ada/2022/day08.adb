with AOC; use AOC;

function Day08 (F : Aoc_File) return Solution is
  type Tree_Viewing_Result is record
    Distance : Positive;
    Viewable : Boolean;
  end record;

  function Tree_Viewing_Distance (
    Tree_Heightmap : Rectangle;
    Target_Row, Target_Column, Edge_Row, Edge_Column : Natural ;
    Delta_Row, Delta_Column : Integer
  ) return Tree_Viewing_Result is
    Distance : Positive := 1;
    Row : Natural := Target_Row + Delta_Row;
    Column : Natural := Target_Column + Delta_Column;
  begin
    loop
      if Tree_Heightmap (Row, Column) >= Tree_Heightmap (Target_Row, Target_Column) then
        return (Distance, False);
      end if;
      exit when Row = Edge_Row and Column = Edge_Column;
      Distance := Distance + 1;
      Row := Row + Delta_Row;
      Column := Column + Delta_Column;
    end loop;
    return (Distance, True);
  end Tree_Viewing_Distance;

  Tree_Heightmap : Rectangle := Read_Rectangle (F);
  Trees_Visible : Natural := 0;
  Best_Scenic_Score : Natural := 0;
begin
  for Row in Tree_Heightmap'Range(1) loop
    if Row = Tree_Heightmap'First(1) or Row = Tree_Heightmap'Last(1) then
      Trees_Visible := Trees_Visible + Tree_Heightmap'Length(2);
    else
      for Column in Tree_Heightmap'Range(2) loop
        if Column = Tree_Heightmap'First(2) or Column = Tree_Heightmap'Last(2) then
          Trees_Visible := Trees_Visible + 1;
        else
          declare
            View_Top : Tree_Viewing_Result := Tree_Viewing_Distance (Tree_Heightmap, Row, Column, Tree_Heightmap'First(1), Column, -1, 0);
            View_Bottom : Tree_Viewing_Result := Tree_Viewing_Distance (Tree_Heightmap, Row, Column, Tree_Heightmap'Last(1), Column, 1, 0);
            View_Left : Tree_Viewing_Result := Tree_Viewing_Distance (Tree_Heightmap, Row, Column, Row, Tree_Heightmap'First(2), 0, -1);
            View_Right : Tree_Viewing_Result := Tree_Viewing_Distance (Tree_Heightmap, Row, Column, Row, Tree_Heightmap'Last(2), 0, 1);
            Scenic_Score : Positive := View_Top.Distance * View_Bottom.Distance * View_Left.Distance * View_Right.Distance;
          begin
            if
              View_Top.Viewable or else
              View_Bottom.Viewable or else
              View_Left.Viewable or else
              View_Right.Viewable
            then
              Trees_Visible := Trees_Visible + 1;
            end if;

            if Scenic_Score > Best_Scenic_Score then
              Best_Scenic_Score := Scenic_Score;
            end if;
          end;
        end if;
      end loop;
    end if;
  end loop;

  return New_Solution (S1 => Trees_Visible'Image, S2 => Best_Scenic_Score'Image);
end Day08;
