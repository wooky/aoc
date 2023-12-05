#include <algorithm>
#include <limits>
#include <numeric>
#include <optional>
#include <ranges>
#include <sstream>
#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{
struct Range {
  uint64_t first, last;

  static Range fromFirstAndLength(uint64_t first, uint64_t length)
  {
    return Range {first, first + length - 1};
  }

  std::optional<Range> intersect(const Range& other) const
  {
    auto first = std::max(this->first, other.first);
    auto last = std::min(this->last, other.last);
    return last < first ? std::nullopt : std::make_optional<Range>(first, last);
  }

  Range delta(int64_t amend)
  {
    return {first + amend, last + amend};
  }
};

struct DestinationResult
{
  Range destinationRange;
  Range processedSourceRange;
};

class Sneed
{
  Range sourceRange;
  int64_t destSourceDelta;

public:
  Sneed(const std::string& line)
  {
    std::istringstream iss {line};
    uint64_t destFirst, sourceFirst, rangeLength;
    iss >> destFirst >> sourceFirst >> rangeLength;
    this->sourceRange = Range::fromFirstAndLength(sourceFirst, rangeLength);
    this->destSourceDelta = (int64_t)destFirst - sourceFirst;
  }

  std::optional<DestinationResult> calcDest(const Range& sourceRange) const
  {
    if (auto processedSourceRange = this->sourceRange.intersect(sourceRange); processedSourceRange)
    {
      return DestinationResult {processedSourceRange.value().delta(destSourceDelta), processedSourceRange.value()};
    }
    return std::nullopt;
  }
};

uint64_t processSource(
  const Range& source,
  const std::vector<std::vector<Sneed>>::const_iterator multiSneed,
  const std::vector<std::vector<Sneed>>::const_iterator multiSneedEnd
)
{
  if (multiSneed == multiSneedEnd)
  {
    return source.first;
  }

  // IMPORTANT! We can't use std::ranges::min() here because if every sneed gets filtered out, min() will read out of bounds.
  // Why is this the case?! Why do I have to do dumb stuff like this????
  auto lowestLocations = *multiSneed
    | std::views::transform([&](const auto& sneed){ return sneed.calcDest(source); })
    | std::views::filter([](const auto& destRes){ return destRes.has_value(); })
    | std::views::transform([&](const auto& destRes){ return processSource(destRes.value().destinationRange, std::next(multiSneed), multiSneedEnd); });

  auto lowestLocation = std::reduce(lowestLocations.begin(), lowestLocations.end(), std::numeric_limits<uint64_t>::max(), [](auto a, auto b){ return std::min(a, b); });
  return lowestLocation;
}

Solution day05(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string(line.begin(), line.end()); });

  auto line = lines.begin();
  std::vector<Range> seeds1, seeds2;
  {
    std::istringstream iss {*line};
    iss.ignore(7); // `seeds: `
    uint64_t seedA, seedB;
    while (!iss.eof())
    {
      iss >> seedA >> seedB;
      seeds1.emplace_back(seedA, seedA);
      seeds1.emplace_back(seedB, seedB);
      seeds2.emplace_back(Range::fromFirstAndLength(seedA, seedB));
    }
    std::advance(line, 2);
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

  auto lowestLocation = std::views::transform([&](const Range& source){ return processSource(source, multiSneeds.cbegin(), multiSneeds.cend()); });
  auto s1 = std::ranges::min(seeds1 | lowestLocation);
  auto s2 = std::ranges::min(seeds2 | lowestLocation);

  return Solution(s1, s2);
}

} // namespace aoc::y2023
