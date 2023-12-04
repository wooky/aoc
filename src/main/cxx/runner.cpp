#include <stdexcept>
#include "aoc.hpp"

namespace aoc
{
  
namespace y2023 { extern aoc::Solution run(const std::string& input, uint16_t day); }

} // namespace aoc

extern "C" aoc::Solution run(const char* input, uint16_t year, uint16_t day)
{
  switch (year)
  {
    case 2023: return aoc::y2023::run(input, day);
    default: throw std::runtime_error("Invalid year");
  }
}
