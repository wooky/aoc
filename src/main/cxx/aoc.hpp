#pragma once

#include <stdexcept>
#include <string>

namespace aoc
{
  
class Solution
{
  const char *s1, *s2;

public:
  template<typename S1, typename S2>
  Solution(S1 s1, S2 s2)
  {
    this->s1 = (new std::string(std::to_string(s1)))->c_str();
    this->s2 = (new std::string(std::to_string(s2)))->c_str();
  }
};

template<int Year, int Day> Solution run(const std::string& input);

template<int Year>
inline Solution delegateRun(const std::string& input, int day)
{
  constexpr int year = Year;
  auto err = std::string("Invalid day for ") + std::to_string(year);
  throw std::runtime_error(err);
}

template<int Year, int Today, int... Days>
inline Solution delegateRun(const std::string& input, int day)
{
  constexpr int today = Today;
  if (day == today)
  {
    return run<Year, Today>(input);
  }
  return delegateRun<Year, Days...>(input, day);
}

} // namespace aoc
