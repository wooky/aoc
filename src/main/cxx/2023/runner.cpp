#include "../aoc.hpp"

namespace aoc::y2023
{
  
extern aoc::Solution day01(const std::string& input);
extern aoc::Solution day02(const std::string& input);

aoc::Solution run(const std::string& input, uint16_t day)
{
  switch (day)
  {
  case 1: return day01(input);
  case 2: return day02(input);
  default: throw "Invalid day for 2023";
  }
}

} // namespace aoc::y2023
