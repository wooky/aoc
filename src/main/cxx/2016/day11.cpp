#include "../aoc.hpp"

#include <array>
#include <bitset>
#include <deque>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

namespace aoc
{
namespace y2016
{
struct Floor
{
  // std::set<char> generators;
  // std::set<char> microchips;
  unsigned char generators, microchips;

  bool isEmpty()
  {
    // return generators.empty() && microchips.empty();
    return generators == 0 && microchips == 0;
  }

  bool isValid()
  {
    // if (generators.empty())
    // {
    //   return true;
    // }
    // for (const auto& microchip : microchips)
    // {
    //   if (generators.find(microchip) == generators.end())
    //   {
    //     return false;
    //   }
    // }
    // return true;
    return generators == 0 || (microchips & ~generators) == 0;
  }
};
bool operator<(const Floor& l, const Floor& r)
{
  return l.generators < r.generators || l.microchips < r.microchips;
}

using Building = std::array<Floor, 4>;
bool operator<(const Building& l, const Building& r)
{
  return l[0] < r[0] || l[1] < r[1] || l[2] < r[2] || l[3] < r[3];
}

struct State
{
  Building current;
  int floorNumber;
  int steps;

  bool isDone()
  {
    return current[0].isEmpty() && current[1].isEmpty() && current[2].isEmpty();
  }

  bool isValid()
  {
    return current[floorNumber].isValid();
  }

  // std::vector<State> createNextStates(std::initializer_list<char> generatorsToMove, std::initializer_list<char> microchipsToMove)
  std::vector<State> createNextStates(unsigned char generatorsToMove, unsigned char microchipsToMove)
  {
    auto newFloor = current[floorNumber];
    // for (auto g : generatorsToMove)
    // {
    //   newFloor.generators.erase(g);
    // }
    newFloor.generators &= ~generatorsToMove;
    // for (auto m : microchipsToMove)
    // {
    //   newFloor.microchips.erase(m);
    // }
    newFloor.microchips &= ~microchipsToMove;
    if (!newFloor.isValid())
    {
      return {};
    }

    std::vector<State> nextStates;
    appendNextState(nextStates, floorNumber + 1, newFloor, generatorsToMove, microchipsToMove);
    appendNextState(nextStates, floorNumber - 1, newFloor, generatorsToMove, microchipsToMove);
    return std::move(nextStates);
  }

  // void appendNextState(std::vector<State>& nextStates, int nextFloorNumber, const Floor& thisNewFloor, std::initializer_list<char> generatorsToMove, std::initializer_list<char> microchipsToMove)
  void appendNextState(std::vector<State>& nextStates, int nextFloorNumber, const Floor& thisNewFloor, unsigned char generatorsToMove, unsigned char microchipsToMove)
  {
    if (nextFloorNumber < 0 || nextFloorNumber >= std::tuple_size_v<Building>)
    {
      return;
    }
    auto newState = *this;
    newState.steps++;
    newState.current[floorNumber] = thisNewFloor;
    newState.floorNumber = nextFloorNumber;
    // std::stringstream ss;
    // ss << steps << ": ";
    // for (auto g : generatorsToMove)
    // {
    //   newState.current[nextFloorNumber].generators.insert(g);
    //   // ss << g << "G ";
    // }
    newState.current[nextFloorNumber].generators |= generatorsToMove;
    // for (auto m : microchipsToMove)
    // {
    //   newState.current[nextFloorNumber].microchips.insert(m);
    //   // ss << m << "M ";
    // }
    newState.current[nextFloorNumber].microchips |= microchipsToMove;
    // std::cout << ss.str() << floorNumber << "->" << nextFloorNumber << "\n";
    // std::cout << steps << ": " << std::bitset<2>(generatorsToMove) << ' ' << std::bitset<2>(microchipsToMove) << ' ' << floorNumber << "->" << nextFloorNumber << "\n";
    nextStates.push_back(std::move(newState));
  }
};

class BuildingIterator
{
private:
  std::deque<State> states;
  std::set<Building> seen;

public:
  BuildingIterator(State&& state)
  {
    states.push_back(std::move(state));
  }

  int run()
  {
    int result = 0;
    for (; result == 0; result = tick()) {}
    return result;
  }

private:
  int tick()
  {
    auto& state = states.front();
    // std::cout << "hi " << std::bitset<2>(state.current[state.floorNumber].generators) << " " << std::bitset<2>(state.current[state.floorNumber].microchips) << "\n";
    if (state.isDone())
    {
      return state.steps;
    }
    if (seen.find(state.current) == seen.end() && state.isValid())
    {
      auto& floor = state.current[state.floorNumber];
      // for (auto g1 = floor.generators.begin(); g1 != floor.generators.end(); ++g1)
      for (unsigned char g1 = 1; g1 < 128; g1 <<= 1)
      {
        if ((floor.generators & g1) == 0)
        {
          continue;
        }
        // appendNextStates(std::move(state.createNextStates({*g1}, {})));
        appendNextStates(std::move(state.createNextStates(g1, 0)));
        // for (auto g2 = std::next(g1); g2 != floor.generators.end(); ++g2)
        for (auto g2 = g1 << 1; g2 < 128; g2 <<= 1)
        {
          // appendNextStates(std::move(state.createNextStates({*g1, *g2}, {})));
          if ((floor.generators & g2) != 0)
          {
            appendNextStates(std::move(state.createNextStates(g1 | g2, 0)));
          }
        }
        // for (auto m = floor.microchips.begin(); m != floor.microchips.end(); ++m)
        for (unsigned char m = 1; m < 128; m <<= 1)
        {
          // appendNextStates(std::move(state.createNextStates({*g1}, {*m})));
          if ((floor.microchips & m) != 0)
          {
            appendNextStates(std::move(state.createNextStates(g1, m)));
          }
        }
      }
      // for (auto m1 = floor.microchips.begin(); m1 != floor.microchips.end(); ++m1)
      for (unsigned char m1 = 1; m1 < 128; m1 <<= 1)
      {
        if ((floor.microchips & m1) == 0)
        {
          continue;
        }
        appendNextStates(std::move(state.createNextStates(0, m1)));
        // for (auto m2 = std::next(m1); m2 != floor.microchips.end(); ++m2)
        for (unsigned char m2 = m1 << 1; m2 < 128; m2 <<= 1)
        {
          // appendNextStates(std::move(state.createNextStates({}, {*m1, *m2})));
          if ((floor.microchips & m2) != 0)
          {
            appendNextStates(std::move(state.createNextStates(0, m1 | m2)));
          }
        }
      }
    }
    states.pop_front();
    return 0;
  }

private:
  void appendNextStates(std::vector<State> nextStates)
  {
    for (auto& s : nextStates)
    {
      states.push_back(std::move(s));
    }
  }
};

} // namespace y2016
using namespace y2016;

template<>
Solution run<2016, 11>(const std::string& input)
{
  constexpr int thulium = 1, plutonium = 2, strontium = 4, promethium = 8, ruthenium = 16;
  State state {
    {
      {
        {thulium | plutonium | strontium, thulium},
        {0, plutonium | strontium},
        {promethium | ruthenium, promethium | ruthenium},
        {0, 0}
      }
    }
  };
  BuildingIterator iter(std::move(state));
  auto s1 = iter.run();

  return Solution(s1, 0);
}

} // namespace aoc
