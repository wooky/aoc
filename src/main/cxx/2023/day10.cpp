#include <map>
#include <queue>
#include <set>
#include "../aoc.hpp"

namespace aoc::y2023
{

class PipeMap
{
private:
  struct Node
  {
    size_t index;
    uint32_t distance;
  };

  enum Direction {
    NORTH = 1,
    SOUTH = 2,
    EAST = 4,
    WEST = 8,
  };

  const std::string& _input;
  const std::size_t _lineSize;
  const std::size_t _startPos;
  std::set<size_t> _processedNodes;
  std::queue<Node> _unprocessedNodes;
  static const std::map<char, uint8_t> _directions;

public:
  PipeMap(const std::string& input)
  : _input(input),
    _lineSize(input.find('\n') + 1), // line size includes newline character
    _startPos(input.find('S'))
  {
    // Do nothing.
  }

  uint32_t calcFurthestDistance()
  {
    _unprocessedNodes.push({_startPos, 0});
    uint32_t furthestDistance = 0;
    for (; !_unprocessedNodes.empty(); _unprocessedNodes.pop())
    {
      const auto node = _unprocessedNodes.front();
      _processedNodes.insert(node.index);
      furthestDistance = std::max(furthestDistance, node.distance);
      tryQueuingNode(node.index - _lineSize, SOUTH, node.distance);
      tryQueuingNode(node.index - 1, EAST, node.distance);
      tryQueuingNode(node.index + 1, WEST, node.distance);
      tryQueuingNode(node.index + _lineSize, NORTH, node.distance);
    }
    return furthestDistance;
  }

private:
  void tryQueuingNode(size_t idx, uint8_t directionTowards, uint32_t prevDistance)
  {
    if (
      idx < 0 || idx >= _input.size() ||
      _processedNodes.find(idx) != _processedNodes.end() ||
      (_directions.at(_input[idx]) & directionTowards) == 0
    )
    {
      return;
    }
    
    _unprocessedNodes.emplace(idx, prevDistance + 1);
  }
};

decltype(PipeMap::_directions) PipeMap::_directions = {
  {'|', NORTH | SOUTH},
  {'-', EAST | WEST},
  {'L', NORTH | EAST},
  {'J', NORTH | WEST},
  {'7', SOUTH | WEST},
  {'F', SOUTH | EAST},
  {'.', 0},
  {'S', 0},
  {'\n', 0},
};
  
Solution day10(const std::string& input)
{
  PipeMap pipeMap { input };
  const auto s1 = pipeMap.calcFurthestDistance();
  return {s1, 0};
}

} // namespace aoc::y2023
