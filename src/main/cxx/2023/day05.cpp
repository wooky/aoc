#include <iostream>
#include <limits>
#include <optional>
#include <ranges>
#include <sstream>
#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{
  
class Sneed
{
  uint64_t sourceStart, sourceEnd;
  uint64_t destStart;

public:
  Sneed(const std::string& line)
  {
    std::istringstream iss {line};
    uint64_t rangeLength;
    iss >> this->destStart >> this->sourceStart >> rangeLength;
    this->sourceEnd = this->sourceStart + rangeLength;
  }

  std::optional<uint64_t> calcDest(uint64_t source) const
  {
    if (source >= sourceStart && source <= sourceEnd)
    {
      return std::make_optional(source - sourceStart + destStart);
    }
    return std::nullopt;
  }
};

Solution day05(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string(line.begin(), line.end()); });

  auto line = lines.begin();
  std::vector<uint64_t> seeds;
  {
    std::istringstream iss {*line};
    iss.ignore(7); // `seeds: `
    uint64_t seed;
    while (!iss.eof())
    {
      iss >> seed;
      seeds.push_back(seed);
    }
    line++; line++;
  }

  std::vector<std::vector<Sneed>> multiSneeds;
  std::vector<Sneed> currentSneeds;
  for (; line != lines.end(); ++line)
  {
    if ((*line).back() == ':')
    {
      continue;
    }
    if ((*line).empty())
    {
      multiSneeds.push_back(currentSneeds);
      currentSneeds = decltype(currentSneeds)();
      continue;
    }
    currentSneeds.emplace_back(*line);
  }

  auto lowestLocation = std::numeric_limits<uint64_t>::max();
  for (auto source : seeds)
  {
    for (const auto& multiSneed : multiSneeds)
    {
      // std::cout << source << " ";
      for (const auto& sneed : multiSneed)
      {
        if (auto dest = sneed.calcDest(source); dest)
        {
          source = dest.value();
          break;
        }
      }
    }
    // std::cout << source << "\n";
    lowestLocation = std::min(lowestLocation, source);
  }

  return Solution(lowestLocation, 0);
}

} // namespace aoc::y2023
