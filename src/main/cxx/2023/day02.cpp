#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <numeric>
#include <ranges>
#include <regex>
#include <vector>

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
  Game(const std::string& line)
  {
    std::smatch match;
    if (!std::regex_search(line, match, reGame))
    {
      return;
    }
    number = std::stoi(match[1].str());

    auto cubeIter = std::sregex_iterator(match.suffix().first, line.cend(), reCube);
    auto lineEnd = std::sregex_iterator();
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
    auto cubeQty = highestCubes | std::views::transform([](auto& cube){ return cube.second; });
    return std::reduce(cubeQty.begin(), cubeQty.end(), 1, std::multiplies());
  }
};
const std::regex Game::reGame { "Game (\\d+):" };
const std::regex Game::reCube { " (\\d+) (\\w+)[,;]?" };

} // namespace aoc::y2023
  
int main()
{
  std::ifstream file("../../../../input/2023/day02.txt");
  std::string line;
  const aoc::y2023::CubeSet maximums {{"red", 12}, {"green", 13}, {"blue", 14}};
  uint32_t s1 = 0, s2 = 0;
  while (std::getline(file, line))
  {
    aoc::y2023::Game game { line };
    s1 += game.isGamePossible(maximums);
    s2 += game.calcPower();
  }
  std::cout << s1 << "\n" << s2 << "\n";
}
