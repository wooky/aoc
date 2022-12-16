with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with AOC; use AOC;
with AOC.Coordinates; use AOC.Coordinates;

function Day12 (F : Aoc_File) return Solution is
  function Dijksta (
    Height_Map: Rectangle;
    Start, Goal: Coordinate;
    New_Distance_Maker: access function (Prev : Natural; Next : Character) return Natural
  ) return Positive is
    package Node_Distance_Container is new Ada.Containers.Hashed_Maps (
      Key_Type => Coordinate,
      Element_Type => Natural,
      Hash => Hash,
      Equivalent_Keys => "="
    );
    Distances : Node_Distance_Container.Map;

    function Get_Priority (C : Coordinate) return Natural is
    begin
      return Distances (C);
    end Get_Priority;

    package Coord_Queue is new Ada.Containers.Synchronized_Queue_Interfaces (
      Element_Type => Coordinate
    );
    package Node_Queue_Container is new Ada.Containers.Unbounded_Priority_Queues (
      Queue_Interfaces => Coord_Queue,
      Queue_Priority => Natural,
      Get_Priority => Get_Priority,
      Before => "<"
    );
    Queue : Node_Queue_Container.Queue;
    
  begin
    Distances.Insert (Start, 0);
    Queue.Enqueue (Start);

    while Queue.Current_Use /= 0 loop
      declare
        Current : Coordinate;
      begin
        Queue.Dequeue (Current);
        if Current = Goal then
          return Distances (Current);
        end if;

        for Neighbor of Current.Direct_Neighbors loop
          if
            Neighbor.Row in Height_Map'Range(1) and then
            Neighbor.Column in Height_Map'Range(2) and then (
              (
                Height_Map (Current.Row, Current.Column) in 'a' .. 'z' and then
                Height_Map (Neighbor.Row, Neighbor.Column) in 'a' .. Character'Val (Character'Pos (Height_Map (Current.Row, Current.Column)) + 1)
              ) or else
              (Height_Map (Current.Row, Current.Column) = 'S' and then Height_Map (Neighbor.Row, Neighbor.Column) in 'a' .. 'b') or else
              (Height_Map (Current.Row, Current.Column) in 'y' .. 'z' and then Height_Map (Neighbor.Row, Neighbor.Column) = 'E')
            )
          then
            declare
              New_Distance : Natural := New_Distance_Maker (Distances (Current), Height_Map (Neighbor.Row, Neighbor.Column));
            begin
              if not Distances.Contains (Neighbor) or else New_Distance < Distances (Neighbor) then
                Distances.Include (Neighbor, New_Distance);
                Queue.Enqueue (Neighbor);
              end if;
            end;
          end if;
        end loop;
      end;
    end loop;

    raise Program_Error with "Cannot find path!";
  end Dijksta;

  function Increment (Prev : Natural; Next : Character) return Natural is
  begin
    return Prev + 1;
  end Increment;

  function Increment_If_Not_Near_Start (Prev : Natural; Next : Character) return Natural is
  begin
    if Prev = 0 and Next = 'a' then
      return 0;
    end if;
    return Prev + 1;
  end Increment_If_Not_Near_Start;

  Height_Map : Rectangle := Read_Rectangle (F);
  Start : Coordinate := Find (Height_Map, 'S');
  Goal : Coordinate := Find (Height_Map, 'E');
begin
  return New_Solution (
    S1 => Dijksta (Height_Map, Start, Goal, Increment'Access)'Image,
    S2 => Dijksta (Height_Map, Start, Goal, Increment_If_Not_Near_Start'Access)'Image
  );
end Day12;
