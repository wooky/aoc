#include <numeric>
#include <ranges>
#include "../aoc.hpp"

namespace aoc::y2023
{

Solution day15(const std::string& input)
{
  auto hashes = input
    | std::views::split(',')
    | std::views::transform([](const auto& step) {
      return std::accumulate(step.begin(), step.end(), (uint8_t)0, [](const auto& prev, const auto& curr) { return (prev + curr) * 17; });
    });
  const auto s1 = std::accumulate(hashes.begin(), hashes.end(), 0u);
  return {s1, 0};
}

} // namespace aoc::y2023
