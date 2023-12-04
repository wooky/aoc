#include <numeric>
#include <ranges>
#include <set>
#include <sstream>
#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{
  
Solution day04(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string_view(line.begin(), line.end()); })
    | std::views::filter([](const auto& line){ return line.size() != 0; });

  uint8_t cardNumber = 0;
  std::vector<uint32_t> cardInstances;
  uint32_t totalPoints = 0;

  for (const auto& line : lines)
  {
    if (cardNumber <= cardInstances.size())
    {
      cardInstances.push_back(0);
    }
    cardInstances[cardNumber]++;

    auto pos = line.cbegin() + 10;
    std::set<std::string> winningNumbers;
    for (; *pos != '|'; pos += 3)
    {
      winningNumbers.insert(std::string(pos, pos + 2));
    }

    pos += 2;
    uint8_t haveWinningNumbers = 0;
    for (; pos < line.end(); pos += 3)
    {
      if (winningNumbers.contains(std::string(pos, pos + 2)))
      {
        haveWinningNumbers++;
      }
    }
    if (haveWinningNumbers > 0)
    {
      totalPoints += 1 << (haveWinningNumbers - 1);

      for (uint8_t idx = cardNumber + 1; idx < cardNumber + 1 + haveWinningNumbers; idx++)
      {
        if (idx <= cardInstances.size())
        {
          cardInstances.push_back(0);
        }
        cardInstances[idx] += cardInstances[cardNumber];
      }
    }

    cardNumber++;
  }

  auto totalInstances = std::accumulate(cardInstances.cbegin(), cardInstances.cend(), 0);
  return Solution(totalPoints, totalInstances);
}

} // namespace aoc::y2023
