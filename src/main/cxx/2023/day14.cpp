#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{

Solution day14(const std::string& input)
{
  const auto columns = input.find('\n');
  uint8_t row = input.size() / (columns + 1);
  std::vector<uint8_t> rocks(columns, row);
  uint32_t load = 0;
  for (size_t idx = 0, column = 0; idx < input.size(); idx++, column++)
  {
    switch (input[idx])
    {
      case '.': break;
      case 'O':
        load += rocks[column];
        rocks[column]--;
        break;
      case '#':
        rocks[column] = row - 1;
        break;
      case '\n':
        row--;
        column = -1;
        break;
      default: throw "unreachable";
    }
  }
  return {load, 0};
}

} // namespace aoc::y2023
