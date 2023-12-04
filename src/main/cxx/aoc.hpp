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
    this->s1 = std::to_string(s1).c_str();
    this->s2 = std::to_string(s2).c_str();
  }
};

} // namespace aoc
