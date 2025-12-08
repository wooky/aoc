#include <map>
#include <ranges>
#include <regex>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{
class Day01
{
private:
  const std::regex reDigits { "[0-9]" };
  const std::regex reWords { "[0-9]|one|two|three|four|five|six|seven|eight|nine" };
  const std::map<std::string, uint32_t> mapWords {
    {"one", 1},
    {"two", 2},
    {"three", 3},
    {"four", 4},
    {"five", 5},
    {"six", 6},
    {"seven", 7},
    {"eight", 8},
    {"nine", 9},
  };

  uint32_t computeDigit(const std::string& s)
  {
    if (auto digit = mapWords.find(s); digit != mapWords.end())
    {
      return digit->second;
    }
    return s[0] - '0';
  }

  uint32_t getCalibration(const std::string_view& line, const std::regex& re)
  {
    std::cmatch firstMatch, lastMatch, match;
    for (
      auto lineIter = line.cbegin();
      std::regex_search(lineIter, line.cend(), match, re);
      lineIter = (*match.begin()).first + 1
    )
    {
      if (!firstMatch.ready())
      {
        firstMatch = match;
      }
      lastMatch = match;
    }

    if (!firstMatch.ready())
    {
      return 0;
    }
    return computeDigit(firstMatch.str()) * 10 + computeDigit(lastMatch.str());
  }

public:
  inline uint32_t getDigitCalibration(const std::string_view& line)
  {
    return getCalibration(line, reDigits);
  }

  inline uint32_t getWordCalibration(const std::string_view& line)
  {
    return getCalibration(line, reWords);
  }
};
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 1>(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string_view(&*line.begin(), std::ranges::distance(line)); });
  Day01 problem;
  uint32_t s1 = 0, s2 = 0;

  for (const auto line : lines)
  {
    s1 += problem.getDigitCalibration(line);
    s2 += problem.getWordCalibration(line);
  }

  return aoc::Solution(s1, s2);
}

} // namespace aoc
