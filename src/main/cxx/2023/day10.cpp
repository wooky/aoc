#include <map>
#include <queue>
#include <set>
#include "../aoc.hpp"

namespace aoc::y2023
{

class PipeMap
{
private:
  struct Coord
  {
    int16_t row, col;

    Coord operator+(const Coord& rhs) const
    {
      return {static_cast<int16_t>(row + rhs.row), static_cast<int16_t>(col + rhs.col)};
    }

    friend auto operator<=>(const Coord&, const Coord&) = default;
  };

  struct Node
  {
    Coord coord;
    uint16_t distance;
  };

  enum Direction {
    NORTH = 1,
    SOUTH = 2,
    EAST = 4,
    WEST = 8,
  };

  std::vector<std::vector<char>> _pipes;
  uint16_t _columns;
  Coord _startPos;
  std::set<Coord> _pipePieces;
  std::queue<Node> _unprocessedNodes;
  static const std::map<char, uint8_t> _directions;

public:
  PipeMap(const std::string& input)
  {
    const auto lineSize = input.find('\n');
    _columns = lineSize * 2 - 1;
    const auto lineCount = input.size() / (lineSize + 1);
    const auto rows = lineCount * 2 - 1;

    for (int16_t lineNumber = 0, rowIdx = 0; lineNumber < lineCount; lineNumber++, rowIdx += 2)
    {
      if (rowIdx != 0)
      {
        _pipes.emplace_back(_columns, '.');
      }
      
      auto& row = _pipes.emplace_back(_columns, '.');
      for (int16_t idx = (lineCount + 1) * lineNumber, colIdx = 0; colIdx < _columns; idx++, colIdx += 2)
      {
        row[colIdx] = input[idx];
        if (input[idx] == 'S')
        {
          _startPos = {rowIdx, colIdx};
        }
      }
      for (int16_t colIdx = 1; colIdx < _columns; colIdx += 2)
      {
        if ((_directions.at(row[colIdx - 1]) & EAST) != 0 && (_directions.at(row[colIdx + 1]) & WEST) != 0)
        {
          row[colIdx] = '-';
        }
      }

      if (rowIdx != 0)
      {
        for (int16_t colIdx = 0; colIdx < _columns; colIdx += 2)
        {
          if ((_directions.at(_pipes[rowIdx - 2][colIdx]) & SOUTH) != 0 && (_directions.at(row[colIdx]) & NORTH) != 0)
          {
            _pipes[rowIdx - 1][colIdx] = '|';
          }
        }
      }
    }
  }

  uint16_t calcFurthestDistance()
  {
    _unprocessedNodes.push({_startPos, 0});
    uint16_t furthestDistance = 0;
    for (; !_unprocessedNodes.empty(); _unprocessedNodes.pop())
    {
      const auto node = _unprocessedNodes.front();
      _pipePieces.insert(node.coord);
      furthestDistance = std::max(furthestDistance, node.distance);
      tryQueuingNode(node.coord + Coord{-1, 0}, SOUTH, node.distance);
      tryQueuingNode(node.coord + Coord{0, -1}, EAST, node.distance);
      tryQueuingNode(node.coord + Coord{0, +1}, WEST, node.distance);
      tryQueuingNode(node.coord + Coord{+1, 0}, NORTH, node.distance);
    }
    return furthestDistance;
  }

private:
  void tryQueuingNode(const Coord& coord, uint8_t directionTowards, uint16_t prevDistance)
  {
    if (
      coord.row < 0 || coord.col < 0 ||
      coord.row >= _pipes.size() || coord.col >= _columns ||
      _pipePieces.find(coord) != _pipePieces.end() ||
      (_directions.at(_pipes[coord.row][coord.col]) & directionTowards) == 0
    )
    {
      return;
    }
    
    // Increment distance on even positions, since odd positions are "virtual"
    _unprocessedNodes.emplace(coord, (coord.row % 2 == 0 && coord.col % 2 == 0) ? prevDistance + 1 : prevDistance);
  }
};

decltype(PipeMap::_directions) PipeMap::_directions = {
  {'|', NORTH | SOUTH},
  {'-', EAST | WEST},
  {'L', NORTH | EAST},
  {'J', NORTH | WEST},
  {'7', SOUTH | WEST},
  {'F', SOUTH | EAST},
  {'.', 0},
  {'S', NORTH | SOUTH | EAST | WEST},
  {'\n', 0},
};
  
Solution day10(const std::string& input)
{
  PipeMap pipeMap { input };
  const auto s1 = pipeMap.calcFurthestDistance();
  return {s1, 0};
}

} // namespace aoc::y2023
