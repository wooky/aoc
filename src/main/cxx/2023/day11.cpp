#include <ranges>
#include <set>
#include <vector>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{

struct Coord
{
  uint8_t row, col;
};
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 11>(const std::string& input)
{
  const auto lineSize = input.find('\n') + 1;
  const auto rows = input.size() / lineSize;
  std::ranges::iota_view<uint8_t, uint8_t> colsRange(0, lineSize - 1), rowsRange(0, rows);
  std::set<uint8_t> emptyCols{ colsRange.begin(), colsRange.end() };
  std::set<uint8_t> emptyRows{ rowsRange.begin(), rowsRange.end() };
  std::vector<Coord> galaxies;

  {
    uint8_t row = 0, col = 0;
    for (size_t idx = 0; idx < input.size(); idx++)
    {
      switch (input[idx])
      {
        case '.':
          col++;
          break;
        case '#':
          galaxies.emplace_back(row, col);
          emptyCols.erase(col);
          emptyRows.erase(row);
          col++;
          break;
        case '\n':
          row++;
          col = 0;
          break;
        default: throw "unreachable";
      }
    }
  }

  uint64_t s1 = 0, s2 = 0;
  for (auto from = galaxies.cbegin(); from != galaxies.cend(); ++from)
  {
    for (auto to = from + 1; to != galaxies.cend(); ++to)
    {
      {
        auto firstCol = from->col, lastCol = to->col;
        if (firstCol > lastCol)
        {
          std::swap(firstCol, lastCol);
        }
        auto emptyColsPassed = emptyCols | std::views::filter([&](const auto col){ return col > firstCol && col < lastCol; });
        auto emptyColCount = std::distance(emptyColsPassed.begin(), emptyColsPassed.end());
        s1 += (lastCol - firstCol) + emptyColCount;
        s2 += (lastCol - firstCol) + emptyColCount * 999'999;
      }

      {
        auto firstRow = from->row, lastRow = to->row;
        auto emptyRowsPassed = emptyRows | std::views::filter([&](const auto row){ return row > firstRow && row < lastRow; });
        auto emptyRowCount = std::distance(emptyRowsPassed.begin(), emptyRowsPassed.end());
        s1 += (lastRow - firstRow) + emptyRowCount;
        s2 += (lastRow - firstRow) + emptyRowCount * 999'999;
      }
    }
  }

  return {s1, s2};
}

} // namespace aoc
