#include "aoc.hpp"

extern "C" aoc::Solution run(const char* input, uint16_t year, uint16_t day)
{
  switch (year)
  {
    case 2023:
      return aoc::delegateRun<
        2023,
        1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
      >(input, day);
    default:
      throw std::runtime_error("Invalid year");
  }
}
