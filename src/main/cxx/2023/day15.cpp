#include <algorithm>
#include <map>
#include <numeric>
#include <ranges>
#include <vector>
#include "../aoc.hpp"

namespace aoc::y2023
{

struct LabelLens
{
  std::string_view label;
  uint8_t lens;
};

class Step
{
public:
  LabelLens labelLens;
  uint8_t operation;
  uint8_t hash, box;

public:
  Step(const std::string_view& token)
  : box(0),
    operation(0)
  {
    for (auto it = token.cbegin(); it < token.cend(); it++)
    {
      switch (const auto c = *it; c)
      {
      case '=':
      {
        finalizeBox(c, token.cbegin(), it);
        const auto next = *++it;
        doHash(hash, next);
        labelLens.lens = next - '0';
        break;
      }
      case '-':
        finalizeBox(c, token.cbegin(), it);
        break;
      default:
        doHash(box, c);
      }
    }
  }

private:
  void finalizeBox(uint8_t operation, const std::string_view::const_iterator& labelFrom, const std::string_view::const_iterator& labelTo)
  {
    this->operation = operation;
    this->labelLens.label = std::string_view(labelFrom, labelTo);
    this->hash = this->box;
    doHash(this->hash, operation);
  }

  static void doHash(uint8_t& res, uint8_t add)
  {
    res = (res + add) * 17;
  }
};

Solution day15(const std::string& input)
{
  auto steps = input
    | std::views::split(',')
    | std::views::transform([](const auto& token) { return Step(std::string_view(token.begin(), token.end())); });
  const auto s1 = std::transform_reduce(steps.begin(), steps.end(), 0, std::plus{}, [](const auto& step) { return step.hash; });

  using Boxes = std::vector<std::vector<LabelLens>>;
  Boxes boxes(256, Boxes::value_type());
  for (const auto& step : steps)
  {
    auto& labelLenses = boxes[step.box];
    auto existingLabelLens = std::find_if(labelLenses.begin(), labelLenses.end(), [&](const auto& labelLens) { return labelLens.label == step.labelLens.label; });
    switch (step.operation)
    {
    case '-':
      if (existingLabelLens != labelLenses.end())
      {
        labelLenses.erase(existingLabelLens);
      }
      break;
    case '=':
      if (existingLabelLens != labelLenses.end())
      {
        *existingLabelLens = step.labelLens;
      }
      else
      {
        labelLenses.insert(existingLabelLens, step.labelLens);
      }
      break;
    default:
      throw "unreachable";
    }
  }

  uint32_t s2 = 0;
  for (uint16_t box = 0; box < boxes.size(); box++)
  {
    const auto& labelLenses = boxes[box];
    for (uint8_t labelLensIdx = 0; labelLensIdx < labelLenses.size(); labelLensIdx++)
    {
      s2 += (box + 1) * (labelLensIdx + 1) * labelLenses[labelLensIdx].lens;
    }
  }

  return {s1, s2};
}

} // namespace aoc::y2023
