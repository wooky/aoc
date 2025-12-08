#include <map>
#include <numeric>
#include <ranges>
#include <regex>
#include <vector>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{
using GearParts = std::map<size_t, std::vector<uint32_t>>;

bool symbolExists(
  const std::string& input,
  std::string::const_iterator begin, std::string::const_iterator end,
  GearParts& gearParts, uint32_t partNumber
)
{
  static std::regex reSymbols {"[^0-9.\\n]"};
  if (begin >= input.cend() || end <= input.cbegin())
  {
    return false;
  }
  
  auto iter = std::sregex_iterator(
    std::max(begin, input.cbegin()),
    std::min(end, input.cend()),
    reSymbols
  );
  auto iterEnd = std::sregex_iterator();
  bool foundSymbols = false;
  for (; iter != iterEnd; ++iter)
  {
    foundSymbols = true;
    const auto strIter = (*iter)[0].first;
    if (*strIter == '*')
    {
      gearParts.try_emplace(strIter - input.cbegin(), GearParts::mapped_type()).first->second.push_back(partNumber);
    }
  }
  return foundSymbols;
}
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 3>(const std::string& input)
{
  uint32_t partNumberSum = 0;
  uint32_t partNumber = 0;
  uint8_t partNumberDigits = 0;
  uint8_t lineLength = input.find('\n') + 1; // \n is considered to be part of the line
  GearParts gearParts;

  for (auto iter = input.cbegin(); iter < input.cend(); iter++)
  {
    if (*iter >= '0' && *iter <= '9')
    {
      partNumber = partNumber*10 + *iter - '0';
      partNumberDigits++;
      continue;
    }
    
    if (partNumber == 0)
    {
      continue;
    }
    
    // At this point, iter is 1 PAST the number
    auto topRight = iter - lineLength;
    auto topLeft = topRight - partNumberDigits - 1;
    auto right = iter;
    auto left = right - partNumberDigits - 1;
    auto bottomRight = iter + lineLength;
    auto bottomLeft = bottomRight - partNumberDigits - 1;
    
    if (
      symbolExists(input, topLeft, topRight + 1, gearParts, partNumber) ||
      symbolExists(input, left, left + 1, gearParts, partNumber) ||
      symbolExists(input, right, right + 1, gearParts, partNumber) ||
      symbolExists(input, bottomLeft, bottomRight + 1, gearParts, partNumber)
    )
    {
      partNumberSum += partNumber;
    }
    
    partNumber = 0;
    partNumberDigits = 0;
  }

  auto gearRatios = gearParts
    | std::views::filter([](auto& gearToPartNumbers){ return gearToPartNumbers.second.size() == 2; })
    | std::views::transform([](auto& gearToPartNumbers){ return gearToPartNumbers.second[0] * gearToPartNumbers.second[1]; });
  auto totalGearRatios = std::accumulate(gearRatios.begin(), gearRatios.end(), 0);

  return Solution(partNumberSum, totalGearRatios);
}

} // namespace aoc
