#include <fstream>
#include <iostream>
#include <map>
#include <regex>

namespace aoc::y2023
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

  uint32_t getCalibration(const std::string& line, const std::regex& re)
  {
    std::smatch firstMatch, lastMatch, match;
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
  inline uint32_t getDigitCalibration(const std::string& line)
  {
    return getCalibration(line, reDigits);
  }

  inline uint32_t getWordCalibration(const std::string& line)
  {
    return getCalibration(line, reWords);
  }
};

} // namespace aoc::y2023

int main()
{
  std::ifstream file("../../../../input/2023/day01.txt");
  std::string line;
  aoc::y2023::Day01 problem;
  uint32_t s1 = 0, s2 = 0;

  while (std::getline(file, line))
  {
    s1 += problem.getDigitCalibration(line);
    s2 += problem.getWordCalibration(line);
  }
  
  std::cout << s1 << "\n" << s2 << "\n";
}
