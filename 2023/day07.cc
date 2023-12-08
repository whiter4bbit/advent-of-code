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
  uint64_t bid{0};
  uint8_t type{0};
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

constexpr std::string_view kLabels{"ABCDEFGHIJKLM"};

template <typename TypeFn>
std::vector<Hand> parse(std::string_view path,
                        const std::unordered_map<char, char> &translation,
                        TypeFn type_fn) {
  std::ifstream s(path);
  std::vector<Hand> hands;
  for (std::string line; std::getline(s, line);) {
    std::stringstream ss(line);
    auto &hand = hands.emplace_back();
    ss >> hand.cards >> hand.bid;
    for (auto &c : hand.cards) {
      c = translation.at(c);
    }
    hand.type = type_fn(hand.cards);
  }
  return hands;
}

uint8_t hand_type(const std::string &cards) {
  std::vector<uint8_t> counts;
  for (auto i{cards.size()}; i > 0; --i) {
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

struct HandTypeP1 {
  uint8_t operator()(const std::string &cards) const {
    return hand_type(cards);
  }
};

struct HandTypeP2 {
  uint8_t operator()(const std::string &cards) const {
    uint8_t best{hand_type(cards)};
    if (cards.find('A') == std::string::npos) {
      return best;
    }
    for (const auto &r : kLabels) {
      auto replaced = cards;
      for (auto &c : replaced) {
        if (c == 'A') {
          c = r;
        }
      }
      best = std::max(best, hand_type(replaced));
    }
    return best;
  }
};

uint64_t solve(const std::vector<Hand> &hands) {
  auto sorted_hands = hands;
  std::sort(sorted_hands.begin(), sorted_hands.end(),
            [](const auto &a, const auto &b) {
              return (a.type != b.type) ? (a.type < b.type) : a.cards < b.cards;
            });
  uint64_t answ{0};
  for (int rank{0}; rank < sorted_hands.size(); ++rank) {
    const auto &hand = sorted_hands[rank];
    answ += hand.bid * (rank + 1);
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << solve(parse("input07-test.txt", kTranslateP1, HandTypeP1()))
            << std::endl;
  std::cout << solve(parse("input07.txt", kTranslateP1, HandTypeP1()))
            << std::endl;
  std::cout << solve(parse("input07-test.txt", kTranslateP2, HandTypeP2()))
            << std::endl;
  std::cout << solve(parse("input07.txt", kTranslateP2, HandTypeP2()))
            << std::endl;
  return 0;
}