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

  enum Direction {
    NORTH = 1,
    SOUTH = 2,
    EAST = 4,
    WEST = 8,
  };

  class DistanceCalculator
  {
    struct Node
    {
      Coord coord;
      uint16_t distance;
    };

    std::queue<Node> _unprocessedNodes;
    PipeMap& _pipeMap;

  public:
    DistanceCalculator(PipeMap& pipeMap)
    : _pipeMap(pipeMap)
    {
      // Do nothing.
    }

    uint16_t calcFurthestDistance()
    {
      _unprocessedNodes.push({_pipeMap._startPos, 0});
      uint16_t furthestDistance = 0;
      for (; !_unprocessedNodes.empty(); _unprocessedNodes.pop())
      {
        const auto& node = _unprocessedNodes.front();
        _pipeMap._pipePieces.insert(node.coord);
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
        coord.row >= _pipeMap._pipes.size() || coord.col >= _pipeMap._columns ||
        _pipeMap._pipePieces.find(coord) != _pipeMap._pipePieces.end() ||
        (_directions.at(_pipeMap._pipes[coord.row][coord.col]) & directionTowards) == 0
      )
      {
        return;
      }
      
      // Increment distance on even positions, since odd positions are "virtual"
      _unprocessedNodes.emplace(coord, (coord.row % 2 == 0 && coord.col % 2 == 0) ? prevDistance + 1 : prevDistance);
    }
  };
  friend DistanceCalculator;

  class EnclosureCalculator
  {
    std::set<Coord> _junkTiles;
    std::queue<Coord> _unprocessedTiles;
    PipeMap& _pipeMap;

  public:
    EnclosureCalculator(PipeMap& pipeMap)
    : _pipeMap(pipeMap)
    {
      // Do nothing.
    }

    uint16_t calcEnclosedTiles()
    {
      floodRow(0);
      floodRow(_pipeMap._pipes.size() - 1);
      floodCol(0);
      floodCol(_pipeMap._columns - 1);
      uint16_t enclosedTiles = 0;
      for (int16_t rowIdx = 0; rowIdx < _pipeMap._pipes.size(); rowIdx += 2)
      {
        for (int16_t colIdx = 0; colIdx < _pipeMap._columns; colIdx += 2)
        {
          Coord coord {rowIdx, colIdx};
          if (_pipeMap._pipePieces.find(coord) == _pipeMap._pipePieces.end() && _junkTiles.find(coord) == _junkTiles.end())
          {
            enclosedTiles++;
          }
        }
      }
      return enclosedTiles;
    }

  private:
    void floodRow(int16_t rowIdx)
    {
      for (int16_t colIdx = 0; colIdx < _pipeMap._columns; colIdx++)
      {
        flood({rowIdx, colIdx});
      }
    }

    void floodCol(int16_t colIdx)
    {
      for (int16_t rowIdx = 1; rowIdx < _pipeMap._pipes.size() - 1; rowIdx++)
      {
        flood({rowIdx, colIdx});
      }
    }

    void flood(const Coord& coord)
    {
      tryQueuingTile(coord);
      for (; !_unprocessedTiles.empty(); _unprocessedTiles.pop())
      {
        const auto& tile = _unprocessedTiles.front();
        tryQueuingTile(tile + Coord{-1, 0});
        tryQueuingTile(tile + Coord{0, -1});
        tryQueuingTile(tile + Coord{0, +1});
        tryQueuingTile(tile + Coord{+1, 0});
      }
    }

    void tryQueuingTile(const Coord& coord)
    {
      if (
        coord.row < 0 || coord.col < 0 ||
        coord.row >= _pipeMap._pipes.size() || coord.col >= _pipeMap._columns ||
        _pipeMap._pipePieces.find(coord) != _pipeMap._pipePieces.end() ||
        _junkTiles.find(coord) != _junkTiles.end()
      )
      {
        return;
      }

      _unprocessedTiles.push(coord);
      _junkTiles.insert(coord);
    }
  };
  friend EnclosureCalculator;

  std::vector<std::vector<char>> _pipes;
  uint16_t _columns;
  Coord _startPos;
  std::set<Coord> _pipePieces;
  
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
      for (int16_t idx = (lineSize + 1) * lineNumber, colIdx = 0; colIdx < _columns; idx++, colIdx += 2)
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

  inline uint16_t calcFurthestDistance()
  {
    DistanceCalculator calculator(*this);
    return calculator.calcFurthestDistance();
  }

  inline uint16_t calcEnclosedTiles()
  {
    EnclosureCalculator calculator(*this);
    return calculator.calcEnclosedTiles();
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
};
  
Solution day10(const std::string& input)
{
  PipeMap pipeMap { input };
  const auto s1 = pipeMap.calcFurthestDistance();
  const auto s2 = pipeMap.calcEnclosedTiles();
  return {s1, s2};
}

} // namespace aoc::y2023
