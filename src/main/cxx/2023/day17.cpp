#include <limits>
#include <set>
#include "../aoc.hpp"
#include <iostream>

namespace aoc::y2023
{

struct Coord
{
  int8_t row, col;

  Coord operator+(const Coord& rhs) const
  {
    return {static_cast<int8_t>(row + rhs.row), static_cast<int8_t>(col + rhs.col)};
  }

  friend auto operator<=>(const Coord&, const Coord&) = default;
};

class LavaMap
{
private:
  struct Stats
  {
    uint32_t heatLoss;
    uint16_t penalty;
  };

private:
  const std::string& _map;
  const uint8_t _side;

public:
  LavaMap(const std::string& map)
  : _map(map),
    _side(map.find('\n'))
  {
    // Do nothing.
  }

  uint32_t calcHeatLoss() const
  {
    return doCalcHeatLoss(std::numeric_limits<uint32_t>::max(), 0, {0, 0}, {0, 0}, {});
  }

private:
  uint32_t doCalcHeatLoss(uint32_t bestHeatLoss, uint32_t heatLoss, const Coord& coord, const Coord& lastTurn, const std::set<Coord>& visited) const
  {
    std::cout << "layer " << visited.size() << " heatLoss " << heatLoss << " lastTurn (" << (int)lastTurn.row << "," << (int)lastTurn.col << ")\n";
    auto nextVisited = visited;
    nextVisited.insert(coord);
    for (const auto& delta : {Coord{0, 1}, Coord{1, 0}, Coord{0, -1}, Coord{-1, 0}})
    {
      const auto nextCoord = coord + delta;
      if (
        visited.contains(nextCoord) ||
        nextCoord.row < 0 || nextCoord.col < 0 ||
        nextCoord.row >= _side || nextCoord.col >= _side ||
        (nextCoord.row == lastTurn.row && std::abs(nextCoord.col - lastTurn.col) > 3) ||
        (nextCoord.col == lastTurn.col && std::abs(nextCoord.row - lastTurn.row) > 3)
      )
      {
        continue;
      }

      const auto nextHeatLoss = heatLoss + _map[(_side + 1) * nextCoord.row + nextCoord.col] - '0';
      if (nextHeatLoss >= bestHeatLoss)
      {
        continue;
      }
      if (nextCoord.row == _side - 1 && nextCoord.col == _side - 1)
      {
        return std::min(bestHeatLoss, nextHeatLoss);
      }

      const auto nextLastTurn = (nextCoord.row == lastTurn.row || nextCoord.col == lastTurn.col) ? lastTurn : coord;
      std::cout << "\tcheck (" << (int)nextCoord.row << "," << (int)nextCoord.col << "), bestHeatLoss " << bestHeatLoss << "\n";
      bestHeatLoss = doCalcHeatLoss(bestHeatLoss, nextHeatLoss, nextCoord, nextLastTurn, nextVisited);
    }
    return bestHeatLoss;
  }
};

Solution day17(const std::string& input)
{
  const LavaMap lavaMap(input);
  const auto s1 = lavaMap.calcHeatLoss();

  return {s1, 0};
}

} // namespace aoc::y2023
