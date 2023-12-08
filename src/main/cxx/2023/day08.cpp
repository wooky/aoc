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

  uint32_t s1 = 0;
  {
    std::string_view src {"AAA"}, dest {"ZZZ"};
    while (src != dest)
    {
      const auto& pair = network[src];
      src = directions[s1 % directions.size()] == 'L' ? pair.first : pair.second;
      s1++;
    }
  }

  uint64_t s2 = 1;
  {
    for (const auto& node : network)
    {
      if (node.first[2] == 'A')
      {
        uint32_t steps = 0;
        auto src = node.first;
        while (src[2] != 'Z')
        {
          const auto& pair = network[src];
          src = directions[steps % directions.size()] == 'L' ? pair.first : pair.second;
          steps++;
        }
        s2 = std::lcm(s2, steps);
      }
    }
  }

  return {s1, s2};
}

} // namespace aoc::y2023
