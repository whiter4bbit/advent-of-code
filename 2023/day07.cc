#include <deque>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

struct Hand {
  std::string cards;
  std::string ranked;
  uint64_t bid{0};
};

const std::unordered_map<char, char> kTranslateP1{
    {'2', 'A'}, {'3', 'B'}, {'4', 'C'}, {'5', 'D'}, {'6', 'E'},
    {'7', 'F'}, {'8', 'G'}, {'9', 'H'}, {'T', 'I'}, {'J', 'J'},
    {'Q', 'K'}, {'K', 'L'}, {'A', 'M'},
};

const std::unordered_map<char, char> kTranslateP2{
    {'J', 'A'}, {'2', 'B'}, {'3', 'C'}, {'4', 'D'}, {'5', 'E'},
    {'6', 'F'}, {'7', 'G'}, {'8', 'H'}, {'9', 'I'}, {'T', 'J'},
    {'Q', 'K'}, {'K', 'L'}, {'A', 'M'},
};

constexpr std::string_view kLabels{"AKQJT98765432"};

std::vector<Hand> parse(std::string_view path,
                        const std::unordered_map<char, char> &translation) {
  std::ifstream s(path);
  std::vector<Hand> hands;
  for (std::string line; std::getline(s, line);) {
    std::stringstream ss(line);
    auto &hand = hands.emplace_back();
    ss >> hand.cards >> hand.bid;
    hand.ranked = hand.cards;
    for (auto &c : hand.ranked) {
      c = translation.at(c);
    }
  }
  return hands;
}

uint8_t hand_type(const std::string &cards) {
  std::vector<uint8_t> counts;
  for (int i = 5; i > 0; --i) {
    for (const auto &label : kLabels) {
      auto count = std::count_if(cards.begin(), cards.end(),
                                 [&](const auto &c) { return c == label; });
      if (count == i) {
        counts.emplace_back(count);
      }
    }
  }
  if (counts.front() == 5) {
    return 7;
  } else if (counts.front() == 4) {
    return 6;
  } else if (counts.front() == 3 && counts.back() == 2) {
    return 5;
  } else if (counts.front() == 3) {
    return 4;
  } else if (counts.front() == 2 && counts.at(1) == 2) {
    return 3;
  } else if (counts.front() == 2) {
    return 2;
  }
  return 1;
}

struct HandsOrderP1 {
  virtual uint8_t best_hand_type(const std::string& cards) const {
    return hand_type(cards);
  }
  bool operator()(const Hand &a, const Hand &b) const {
    auto aType = best_hand_type(a.cards);
    auto bType = best_hand_type(b.cards);
    if (aType != bType) {
      return aType < bType;
    }
    return a.ranked < b.ranked;
  }
};

struct HandsOrderP2: public HandsOrderP1 {
  uint8_t best_hand_type(const std::string &cards) const override {
    uint8_t best{hand_type(cards)};
    if (cards.find('J') == std::string::npos) {
      return best;
    }
    for (const auto &r : kLabels) {
      auto replaced = cards;
      for (auto &c : replaced) {
        if (c == 'J') {
          c = r;
        }
      }
      best = std::max(best, hand_type(replaced));
    }
    return best;
  }
};

template <typename OrderFn>
uint64_t solve(const std::vector<Hand> &hands, OrderFn order_fn) {
  auto sorted_hands = hands;
  std::sort(sorted_hands.begin(), sorted_hands.end(), order_fn);
  uint64_t answ{0};
  for (int rank{0}; rank < sorted_hands.size(); ++rank) {
    const auto &hand = sorted_hands[rank];
    answ += hand.bid * (rank + 1);
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << solve(parse("input07-test.txt", kTranslateP1), HandsOrderP1())
            << std::endl;
  std::cout << solve(parse("input07.txt", kTranslateP1), HandsOrderP1())
            << std::endl;
  std::cout << solve(parse("input07-test.txt", kTranslateP2), HandsOrderP2())
            << std::endl;
  std::cout << solve(parse("input07.txt", kTranslateP2), HandsOrderP2())
            << std::endl;
  return 0;
}