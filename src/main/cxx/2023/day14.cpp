#include <algorithm>
#include <numeric>
#include <ranges>
#include <set>
#include <vector>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{

// Forward declaration
struct RockGrid;
void slideNorth(RockGrid&);

struct Coord
{
  int8_t row, col;

  friend auto operator<=>(const Coord&, const Coord&) = default;
};

using RoundedRocks = std::vector<Coord>;
using CubeRocks = std::set<Coord>;

struct RockGrid
{
  RoundedRocks rounded;
  CubeRocks cubes;
  uint8_t sideSize;

  RockGrid(const std::string& input)
  : sideSize(input.find('\n'))
  {
    uint8_t row = 0;
    for (size_t idx = 0, column = 0; idx < input.size(); idx++, column++)
    {
      switch (input[idx])
      {
        case '.': break;
        case 'O':
          rounded.emplace_back(row, column);
          break;
        case '#':
          cubes.emplace(row, column);
          break;
        case '\n':
          row++;
          column = -1;
          break;
        default: throw "unreachable";
      }
    }
  }

  uint32_t load() const
  {
    return std::transform_reduce(rounded.cbegin(), rounded.cend(), 0, std::plus{}, [&](const auto& round) { return sideSize - round.row; });
  }

  void sortEast()
  {
    std::sort(rounded.begin(), rounded.end(), [](const Coord& lhs, const Coord& rhs) {
      auto res = lhs.col <=> rhs.col;
      return (res == 0) ? lhs.row < rhs.row : res > 0;
    });
  }
};

void slideNorth(RockGrid& rockGrid)
{
  std::vector<int8_t> blockages(rockGrid.sideSize, -1);
  std::sort(rockGrid.rounded.begin(), rockGrid.rounded.end(), [](const auto& lhs, const auto& rhs) {
    auto res = lhs.row <=> rhs.row;
    return (res == 0) ? lhs.col < rhs.col : res < 0;
  });
  for (auto& round : rockGrid.rounded)
  {
    for (; round.row > blockages[round.col] && !rockGrid.cubes.contains(round); round.row--) {}
    round.row++;
    blockages[round.col] = round.row;
  }
}

void slideWest(RockGrid& rockGrid)
{
  std::vector<int8_t> blockages(rockGrid.sideSize, -1);
  std::sort(rockGrid.rounded.begin(), rockGrid.rounded.end(), [](const auto& lhs, const auto& rhs) {
    auto res = lhs.col <=> rhs.col;
    return (res == 0) ? lhs.row < rhs.row : res < 0;
  });
  for (auto& round : rockGrid.rounded)
  {
    for (; round.col > blockages[round.row] && !rockGrid.cubes.contains(round); round.col--) {}
    round.col++;
    blockages[round.row] = round.col;
  }
}

void slideSouth(RockGrid& rockGrid)
{
  std::vector<int8_t> blockages(rockGrid.sideSize, rockGrid.sideSize);
  std::sort(rockGrid.rounded.begin(), rockGrid.rounded.end(), [](const auto& lhs, const auto& rhs) {
    auto res = lhs.row <=> rhs.row;
    return (res == 0) ? lhs.col < rhs.col : res > 0;
  });
  for (auto& round : rockGrid.rounded)
  {
    for (; round.row < blockages[round.col] && !rockGrid.cubes.contains(round); round.row++) {}
    round.row--;
    blockages[round.col] = round.row;
  }
}

void slideEast(RockGrid& rockGrid)
{
  std::vector<int8_t> blockages(rockGrid.sideSize, rockGrid.sideSize);
  rockGrid.sortEast();
  for (auto& round : rockGrid.rounded)
  {
    for (; round.col < blockages[round.row] && !rockGrid.cubes.contains(round); round.col++) {}
    round.col--;
    blockages[round.row] = round.col;
  }
}
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 14>(const std::string& input)
{
  RockGrid rockGrid {input};
  uint32_t s1, s2;

  {
    RockGrid tempGrid = rockGrid;
    slideNorth(tempGrid);
    s1 = tempGrid.load();
  }

  {
    std::vector<RoundedRocks> eastRocks;
    uint64_t idx = 0, dupeIdx;
    for (;; idx++)
    {
      slideNorth(rockGrid);
      slideWest(rockGrid);
      slideSouth(rockGrid);
      slideEast(rockGrid);

      rockGrid.sortEast();
      if (const auto found = std::find(eastRocks.cbegin(), eastRocks.cend(), rockGrid.rounded); found != eastRocks.cend())
      {
        dupeIdx = std::distance(eastRocks.cbegin(), found);
        break;
      }
      eastRocks.push_back(rockGrid.rounded);
    }

    const auto lastIdx = (1000000000 - dupeIdx) % (idx - dupeIdx) + dupeIdx - 1; // trust me bro
    rockGrid.rounded = eastRocks[lastIdx];
    s2 = rockGrid.load();
  }

  return {s1, s2};
}

} // namespace aoc
