#include <algorithm>
#include <array>
#include <numeric>
#include <ranges>
#include <set>
#include "../aoc.hpp"

namespace aoc
{
namespace y2023
{
using Cards = std::array<uint8_t, 13>;

struct CardCount
{
  uint8_t count;
  bool extraPair = false;

  CardCount(const Cards::const_iterator& begin, const Cards::const_iterator& end)
  {
    const auto& maxElement = std::max_element(begin, end);
    count = *maxElement;
    if (count == 3)
    {
      extraPair = std::find(begin, end, 2) != end;
    }
    else if (count == 2)
    {
      extraPair = std::find(maxElement + 1, end, 2) != end;
    }
  }

  uint8_t getStrength() const
  {
    switch (count)
    {
      case 5: return 6;
      case 4: return 5;
      case 3: return extraPair ? 4 : 3;
      case 2: return extraPair ? 2 : 1;
      case 1: return 0;
      default: throw "unreachable";
    }
  }
};
  
class HandBid
{
protected:
  std::array<uint8_t, 5> _hand;
  uint8_t _strength;

public:
  uint16_t bid;

public:
  HandBid(uint16_t bid)
  : bid(bid)
  {
    // Do nothing.
  }

  bool operator<(const HandBid& other) const
  {
    if (_strength != other._strength)
    {
      return _strength < other._strength;
    }
    return std::lexicographical_compare(_hand.cbegin(), _hand.cend(), other._hand.cbegin(), other._hand.cend());
  }
};

class HandBid1 : public HandBid
{
public:
  HandBid1(const std::string_view& cardsLine, uint16_t bid) : HandBid(bid)
  {
    Cards cards;
    cards.fill(0);
    for (uint8_t idx = 0; idx < 5; idx++)
    {
      uint8_t card;
      switch (auto c = cardsLine[idx])
      {
        case 'A': card = 12; break;
        case 'K': card = 11; break;
        case 'Q': card = 10; break;
        case 'J': card = 9; break;
        case 'T': card = 8; break;
        default: card = c - '2';
      }
      cards[card]++;
      _hand[idx] = card;
    }

    _strength = CardCount(cards.cbegin(), cards.cend()).getStrength();
  }
};

class HandBid2 : public HandBid
{
public:
  HandBid2(const std::string_view& cardsLine, uint16_t bid) : HandBid(bid)
  {
    Cards cards;
    cards.fill(0);
    for (uint8_t idx = 0; idx < 5; idx++)
    {
      uint8_t card;
      switch (auto c = cardsLine[idx])
      {
        case 'A': card = 12; break;
        case 'K': card = 11; break;
        case 'Q': card = 10; break;
        case 'T': card = 9; break;
        case 'J': card = 0; break;
        default: card = c - '1';
      }
      cards[card]++;
      _hand[idx] = card;
    }
    
    CardCount cardCount(cards.cbegin() + 1, cards.cend()); // exclude jokers
    cardCount.count += cards[0]; // jokers
    _strength = cardCount.getStrength();
  }
};

uint64_t calcTotalWinnings(const auto& handBids)
{
  uint64_t totalWinnings = 0;
  uint64_t rank = 1;
  for (const auto& handBid : handBids)
  {
    totalWinnings += rank * handBid.bid;
    rank++;
  }
  return totalWinnings;
}
} // namespace y2023
using namespace y2023;

template<>
Solution run<2023, 7>(const std::string& input)
{
  auto lines = input
    | std::views::split('\n')
    | std::views::transform([](const auto& line){ return std::string_view(line.begin(), line.end()); })
    | std::views::filter([](const auto& line){ return !line.empty(); });

  std::set<HandBid1> handBids1;
  std::set<HandBid2> handBids2;
  for (const auto& line : lines)
  {
    const auto& cardLine = line.substr(0, 5);
    const uint16_t bid = std::stoi(std::string(line.substr(6)));
    handBids1.emplace(cardLine, bid);
    handBids2.emplace(cardLine, bid);
  }

  auto s1 = calcTotalWinnings(handBids1);
  auto s2 = calcTotalWinnings(handBids2);

  return {s1, s2};
}

} // namespace aoc
