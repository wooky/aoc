with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Day02 is
  subtype Elf_Move_Marker is Character range 'A' .. 'C';
  subtype My_Move_Marker is Character range 'X' .. 'Z';

  type Move is (Rock, Paper, Scissors);
  Elf_Moves : constant array (Elf_Move_Marker) of Move := (
    'A' => Rock,
    'B' => Paper,
    'C' => Scissors
  );
  My_Wrong_Moves : constant array (My_Move_Marker) of Move := (
    'X' => Rock,
    'Y' => Paper,
    'Z' => Scissors
  );

  type Strategy is (Lose, Draw, Win);
  My_Strategies : constant array (My_Move_Marker) of Strategy := (
    'X' => Lose,
    'Y' => Draw,
    'Z' => Win
  );

  type Move_Stat is record
    Score : Natural;
    Winning_Move : Move;
    Losing_Move : Move;
  end record;

  Elf_To_My_Move_Stats : constant array (Move) of Move_Stat := (
    Rock => (1, Paper, Scissors),
    Paper => (2, Scissors, Rock),
    Scissors => (3, Rock, Paper)
  );

  Score_Lose : constant Natural := 0;
  Score_Draw : constant Natural := 3;
  Score_Win : constant Natural := 6;

  Wrong_Total_Score : Natural := 0;
  Right_Total_Score : Natural := 0;
begin
  declare
    F : File_Type;
  begin
    Open (F, In_File, "input/2022/day02.txt");
    while not End_Of_File (F) loop
      declare
        Line : String := Get_Line (F);
        Elf_Move : Move := Elf_Moves (Elf_Move_Marker (Line (1)));
        My_Wrong_Move : Move := My_Wrong_Moves (My_Move_Marker (Line (3)));
        My_Strategy : Strategy := My_Strategies (My_Move_Marker (Line (3)));

        Wrong_Win_Score : Natural := Score_Draw;
        My_Right_Move : Move;
        Right_Win_Score : Natural;
      begin
        if My_Wrong_Move = Elf_To_My_Move_Stats (Elf_Move).Losing_Move then
          Wrong_Win_Score := Score_Lose;
        elsif My_Wrong_Move = Elf_To_My_Move_Stats (Elf_Move).Winning_Move then
          Wrong_Win_Score := Score_Win;
        end if;
        Wrong_Total_Score := Wrong_Total_Score + Wrong_Win_Score + Elf_To_My_Move_Stats (My_Wrong_Move).Score;

        case My_Strategy is
          when Lose =>
            My_Right_Move := Elf_To_My_Move_Stats (Elf_Move).Losing_Move;
            Right_Win_Score := Score_Lose;
          when Draw =>
            My_Right_Move := Elf_Move;
            Right_Win_Score := Score_Draw;
          when Win =>
            My_Right_Move := Elf_To_My_Move_Stats (Elf_Move).Winning_Move;
            Right_Win_Score := Score_Win;
        end case;
        Right_Total_Score := Right_Total_Score + Right_Win_Score + Elf_To_My_Move_Stats (My_Right_Move).Score;
      end;
    end loop;
    Close (F);
  end;

  Put_Line (Wrong_Total_Score'Image);
  Put_Line (Right_Total_Score'Image);
end Day02;
