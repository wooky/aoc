#include <algorithm>
#include <numeric>
#include <ranges>
#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{

struct FirstLast
{
  int64_t first;
  int64_t last;

  FirstLast operator+(const FirstLast& rhs) const
  {
    return {first + rhs.first, last + rhs.last};
  }
};

FirstLast predict(std::vector<uint64_t> values)
{
  FirstLast prevValue(values.front(), values.back());
  std::adjacent_difference(values.cbegin(), values.cend(), values.begin());
  const auto targetDiff = values.back();
  const FirstLast nextValue = std::all_of(values.cbegin() + 1, values.cend(), [&](const auto diff) { return diff == targetDiff; })
    ? FirstLast(targetDiff, targetDiff)
    : predict({values.cbegin() + 1, values.cend()});
  return {prevValue.first - nextValue.first, prevValue.last + nextValue.last};
}
  
Solution day09(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::filter([](const auto& line) { return line.size() != 0; })
    | std::views::transform([](const auto& line) {
      auto tokens = line
        | std::views::split(' ')
        | std::views::transform([](const auto& value) { return std::stoi(std::string(value.begin(), value.end())); });
      std::vector<uint64_t> values(tokens.begin(), tokens.end());
      return predict(values);
    });
  const auto range = std::accumulate(lines.begin(), lines.end(), FirstLast(0, 0));

  return {range.last, range.first};
}

} // namespace aoc::y2023
