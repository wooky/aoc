#include <map>
#include <numeric>
#include <ranges>
#include "../aoc.hpp"

namespace aoc::y2023
{
  
Solution day08(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string_view(line.begin(), line.end()); })
    | std::views::filter([](const auto& line){ return !line.empty(); });
  
  const auto& directions = *lines.begin();

  std::map<std::string_view, std::pair<std::string_view, std::string_view>> network;
  for (const auto& line : lines | std::views::drop(1))
  {
    network.emplace(line.substr(0, 3), std::make_pair(line.substr(7, 3), line.substr(12, 3)));
  }

  uint32_t steps = 0;
  std::string_view node {"AAA"}, dest {"ZZZ"};
  while (node != dest)
  {
    const auto& pair = network[node];
    node = directions[steps % directions.size()] == 'L' ? pair.first : pair.second;
    steps++;
  }

  return {steps, 0};
}

} // namespace aoc::y2023
