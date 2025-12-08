#include <algorithm>
#include <numeric>
#include <queue>
#include <ranges>
#include <set>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{

struct SpringRow
{
  std::string springs, springs5;
  std::queue<uint8_t> damaged, damaged5;

  SpringRow(const std::string_view& line)
  {
    const auto space = line.find(' ');
    springs = springs5 = line.substr(0, space);

    auto damagedValues = line.substr(space + 1)
      | std::views::split(',')
      | std::views::transform([](const auto& token) { return static_cast<uint8_t>(std::stoi(std::string(token.begin(), token.end()))); });
    std::deque<uint8_t> damagedDeque(damagedValues.begin(), damagedValues.end()), damagedDeque5(damagedDeque);
    damaged = std::queue(damagedDeque);

    for (uint8_t i = 1; i < 5; i++)
    {
      springs5 += "?" + springs;
      damagedDeque5.insert(damagedDeque5.end(), damagedDeque.begin(), damagedDeque.end());
    }
    damaged5 = std::queue(damagedDeque5);
  }
};

std::set<std::string> calcChoices(const std::string& springs, std::queue<uint8_t> damaged, size_t fillStart = 0, int layer = 0)
{
  const size_t fillCount = damaged.front();
  damaged.pop();
  std::set<std::string> uniqueChoices;
  bool terminate = false;

  for (
    auto fillBegin = springs.cbegin() + fillStart, fillEnd = fillBegin + fillCount;
    !terminate && fillEnd <= springs.cend();
    ++fillBegin, ++fillEnd
  )
  {
    // Skip if current spring is not damaged
    if (*fillBegin == '.')
    {
      continue;
    }

    // We must fill if the current spring is damaged
    terminate = *fillBegin == '#';

    // If the spring after the contiguous group of damaged springs is also damaged, the fill is invalid
    if (fillEnd < springs.end() && *fillEnd == '#')
    {
      continue;
    }

    // If the contiguous group of damaged springs cannot be filled in because there are undamaged springs, the fill is invalid
    const auto cannotFit = std::find(fillBegin, fillEnd, '.');
    if (cannotFit != fillEnd)
    {
      continue;
    }

    // Dupe the springs with the damaged springs filled in
    auto nextSprings = springs;
    const auto nextSpringsBegin = fillBegin - springs.cbegin() + nextSprings.begin();
    const auto nextSpringsEnd = fillEnd - springs.cbegin() + nextSprings.begin();
    std::fill(nextSpringsBegin, nextSpringsEnd, '#');

    // If there are no more damaged springs left in the queue, the sequence is valid only when there are no more trailing damaged springs
    if (damaged.empty())
    {
      if (nextSpringsEnd != nextSprings.end())
      {
        const auto trailingDamaged = std::find(nextSpringsEnd + 1, nextSprings.end(), '#');
        if (trailingDamaged != nextSprings.end())
        {
          continue;
        }
      }
      uniqueChoices.insert(nextSprings);
    }
    else if (nextSpringsEnd != nextSprings.end())
    {
      uniqueChoices.merge(calcChoices(nextSprings, damaged, std::distance(nextSprings.begin(), nextSpringsEnd) + 1, layer + 1));
    }
  }
  return uniqueChoices;
}
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 12>(const std::string& input)
{
  auto springRows = input
    | std::views::split('\n')
    | std::views::filter([](const auto& line) { return line.size() != 0; })
    | std::views::transform([](const auto& line) { return SpringRow(std::string_view(line.begin(), line.end())); });

  const auto s1 = std::transform_reduce(springRows.begin(), springRows.end(), 0, std::plus{}, [](const auto& springRow) { return calcChoices(springRow.springs, springRow.damaged).size(); });
  const auto s2 = std::transform_reduce(springRows.begin(), springRows.end(), 0, std::plus{}, [](const auto& springRow) { return calcChoices(springRow.springs5, springRow.damaged5).size(); });

  return {s1, s2};
}

} // namespace aoc
