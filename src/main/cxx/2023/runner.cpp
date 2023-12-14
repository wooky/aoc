#include <stdexcept>
#include "../aoc.hpp"

namespace aoc::y2023
{
  
extern aoc::Solution day01(const std::string& input);
extern aoc::Solution day02(const std::string& input);
extern aoc::Solution day03(const std::string& input);
extern aoc::Solution day04(const std::string& input);
extern aoc::Solution day05(const std::string& input);
extern aoc::Solution day07(const std::string& input);
extern aoc::Solution day08(const std::string& input);
extern aoc::Solution day09(const std::string& input);
extern aoc::Solution day10(const std::string& input);
extern aoc::Solution day11(const std::string& input);
extern aoc::Solution day12(const std::string& input);
extern aoc::Solution day13(const std::string& input);
extern aoc::Solution day14(const std::string& input);

aoc::Solution run(const std::string& input, uint16_t day)
{
  switch (day)
  {
  case 1: return day01(input);
  case 2: return day02(input);
  case 3: return day03(input);
  case 4: return day04(input);
  case 5: return day05(input);
  case 7: return day07(input);
  case 8: return day08(input);
  case 9: return day09(input);
  case 10: return day10(input);
  case 11: return day11(input);
  case 12: return day12(input);
  case 13: return day13(input);
  case 14: return day14(input);
  default: throw std::runtime_error("Invalid day for 2023");
  }
}

} // namespace aoc::y2023
