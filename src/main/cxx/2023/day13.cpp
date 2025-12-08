#include <cassert>
#include <numeric>
#include <ranges>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{

struct Reflection
{
  uint16_t unsmudged, smudged;

  Reflection(const std::string& pattern)
  : unsmudged(0),
    smudged(0)
  {
    const auto columns = pattern.find('\n');
    const auto columnsIdxDelta = columns + 1;
    const auto rows = (pattern.size() + 1) / columnsIdxDelta;

    for (uint8_t column = 1; column < columns; column++)
    {
      uint8_t errors = 0;
      for (int16_t idxLeftStart = column - 1, idxRightStart = column; idxLeftStart >= 0 && idxRightStart < columns; idxLeftStart--, idxRightStart++)
      {
        for (auto idxLeft = idxLeftStart, idxRight = idxRightStart; idxLeft < pattern.size(); idxLeft += columnsIdxDelta, idxRight += columnsIdxDelta)
        {
          if (pattern[idxLeft] != pattern[idxRight])
          {
            errors++;
          }
        }
      }
      if (errors == 0)
      {
        assert((unsmudged == 0));
        unsmudged = column;
      }
      else if (errors == 1)
      {
        assert((smudged == 0));
        smudged = column;
      }
    }

    for (uint8_t row = 1; row < rows; row++)
    {
      uint8_t errors = 0;
      for (int16_t idxTopStart = (row - 1) * columnsIdxDelta, idxBottomStart = row * columnsIdxDelta; idxTopStart >= 0 && idxBottomStart < pattern.size(); idxTopStart -= columnsIdxDelta, idxBottomStart += columnsIdxDelta)
      {
        for (auto idxTop = idxTopStart, idxBottom = idxBottomStart; idxTop < idxTopStart + columns; idxTop++, idxBottom++)
        {
          if (pattern[idxTop] != pattern[idxBottom])
          {
            errors++;
          }
        }
      }
      if (errors == 0)
      {
        assert((unsmudged == 0));
        unsmudged = row * 100;
      }
      else if (errors == 1)
      {
        assert((smudged == 0));
        smudged = row * 100;
      }
    }

    assert((unsmudged != 0 && smudged != 0));
  }

  Reflection(uint16_t unsmudged, uint16_t smudged)
  : unsmudged(unsmudged),
    smudged(smudged)
  {
    // Do nothing.
  }

  Reflection operator+(const Reflection& rhs) const
  {
    return {static_cast<uint16_t>(unsmudged + rhs.unsmudged), static_cast<uint16_t>(smudged + rhs.smudged)};
  }
};
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 13>(const std::string& input)
{
  const std::string delim {"\n\n"};
  auto groups = std::ranges::split_view(input, delim);
  auto reflections = groups | std::views::transform([](const auto& group) { return Reflection({group.begin(), group.end()}); });
  const auto reflectionSums = std::accumulate(reflections.begin(), reflections.end(), Reflection{0, 0});

  return {reflectionSums.unsmudged, reflectionSums.smudged};
}

} // namespace aoc
