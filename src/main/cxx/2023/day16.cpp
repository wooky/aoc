#include <algorithm>
#include <set>
#include "../aoc.hpp"

namespace aoc::y2023
{
  
struct Coord
{
  int8_t row, col;

  Coord operator+(const Coord& rhs) const
  {
    return {static_cast<int8_t>(row + rhs.row), static_cast<int8_t>(col + rhs.col)};
  }

  Coord& operator+=(const Coord& rhs)
  {
    row += rhs.row;
    col += rhs.col;
    return *this;
  }

  friend auto operator<=>(const Coord&, const Coord&) = default;
};

class MirrorGrid
{
private:
  const std::string& _tiles;
  const uint8_t _size;
  std::set<Coord> _energizedTiles;

public:
  MirrorGrid(const std::string& tiles, uint8_t size)
  : _tiles(tiles),
    _size(size)
  {
    // Do nothing.
  }

  uint16_t calcEnergized(Coord location, Coord delta)
  {
    doCalcEnergized(location, delta);
    return _energizedTiles.size();
  }

private:
  void doCalcEnergized(Coord location, Coord delta)
  {
    while (location.row >= 0 && location.col >= 0 && location.row < _size && location.col < _size)
    {
      switch (_tiles[location.row * (_size + 1) + location.col])
      {
      case '.': break;
      case '/':
        delta = {static_cast<int8_t>(-delta.col), static_cast<int8_t>(-delta.row)};
        break;
      case '\\':
        delta = {delta.col, delta.row};
        break;
      case '|':
        if (delta.col != 0)
        {
          if (_energizedTiles.contains(location))
          {
            return;
          }
          _energizedTiles.insert(location);
          doCalcEnergized(location + Coord{-1, 0}, {-1, 0});
          delta = {1, 0};
        }
        break;
      case '-':
        if (delta.row != 0)
        {
          if (_energizedTiles.contains(location))
          {
            return;
          }
          _energizedTiles.insert(location);
          doCalcEnergized(location + Coord{0, -1}, {0, -1});
          delta = {0, 1};
        }
        break;
      default: throw "unreachable";
      }

      _energizedTiles.insert(location);
      location += delta;
    }
  }
};

Solution day16(const std::string& input)
{
  const uint8_t mirrorSize = input.find('\n');
  const auto s1 = MirrorGrid(input, mirrorSize).calcEnergized({0, 0}, {0, 1});
  uint16_t s2 = 0;
  for (int8_t i = 0; i < mirrorSize; i++)
  {
    s2 = std::max({
      s2,
      MirrorGrid(input, mirrorSize).calcEnergized({i, 0}, {0, 1}),
      MirrorGrid(input, mirrorSize).calcEnergized({0, i}, {1, 0}),
      MirrorGrid(input, mirrorSize).calcEnergized({i, static_cast<int8_t>(mirrorSize - 1)}, {0, -1}),
      MirrorGrid(input, mirrorSize).calcEnergized({static_cast<int8_t>(mirrorSize - 1), i}, {-1, 0}),
    });
  }

  return {s1, s2};
}

} // namespace aoc::y2023
