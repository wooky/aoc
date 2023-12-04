#include <algorithm>
#include <map>
#include <numeric>
#include <ranges>
#include <regex>
#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{

using CubeSet = std::map<std::string, uint8_t>;

class Game
{
private:
  static const std::regex reGame, reCube;

  CubeSet highestCubes;
  uint8_t number = 0;

public:
  Game(const std::string_view& line)
  {
    std::cmatch match;
    if (!std::regex_search(line.cbegin(), line.cend(), match, reGame))
    {
      return;
    }
    number = std::stoi(match[1].str());

    auto cubeIter = std::cregex_iterator(match.suffix().first, line.cend(), reCube);
    auto lineEnd = std::cregex_iterator();
    CubeSet cubeSet;
    for (; cubeIter != lineEnd; ++cubeIter)
    {
      match = *cubeIter;
      uint8_t cubeQty = std::stoi(match[1].str());
      auto iter = highestCubes.try_emplace(match[2].str(), cubeQty).first;
      iter->second = std::max(iter->second, cubeQty);
    }
  }

  uint8_t isGamePossible(const CubeSet& maximums)
  {
    auto exceedsMaximum = std::ranges::any_of(highestCubes, [&](const auto& cube) {
      auto maximumCube = maximums.find(cube.first);
      return maximumCube != maximums.end() && cube.second > maximumCube->second;
    });
    return exceedsMaximum ? 0 : number;
  }

  uint32_t calcPower()
  {
    if (number == 0)
    {
      return 0;
    }
    auto cubeQty = highestCubes | std::views::transform([](auto& cube){ return cube.second; });
    return std::reduce(cubeQty.begin(), cubeQty.end(), 1, std::multiplies());
  }
};
const std::regex Game::reGame { "Game (\\d+):" };
const std::regex Game::reCube { " (\\d+) (\\w+)[,;]?" };
  
aoc::Solution day02(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string_view(&*line.begin(), std::ranges::distance(line)); });
  const aoc::y2023::CubeSet maximums {{"red", 12}, {"green", 13}, {"blue", 14}};
  uint32_t s1 = 0, s2 = 0;
  for (const auto line : lines)
  {
    aoc::y2023::Game game { line };
    s1 += game.isGamePossible(maximums);
    s2 += game.calcPower();
  }
  return aoc::Solution(s1, s2);
}

} // namespace aoc::y2023
