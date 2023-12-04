#include <fstream>
#include <iostream>
#include <vector>

struct Card {
  Card(const std::vector<int> &winning, const std::vector<int> &have)
      : matching(std::count_if(have.begin(), have.end(),
                               [&](auto n) {
                                 return std::find(winning.begin(),
                                                  winning.end(),
                                                  n) != winning.end();
                               })),
        points(matching > 0 ? (1 << (matching - 1)) : 0) {}
  uint64_t matching{0};
  uint64_t points{0};
  uint64_t copies{1};
};

std::vector<Card> parse(std::string_view path) {
  auto read_numbers = [](const auto &line) {
    std::vector<int> numbers;
    for (int i{0}; i < line.size();) {
      if (!std::isdigit(line[i])) {
        ++i;
        continue;
      }
      auto &num = numbers.emplace_back();
      while (std::isdigit(line[i])) {
        num = num * 10 + (line[i++] - '0');
      }
    }
    return numbers;
  };
  std::vector<Card> cards;
  std::fstream s(path);
  for (std::string line; std::getline(s, line);) {
    line = line.substr(line.find(": ") + 2);
    static const std::string kSep = " | ";
    cards.emplace_back(
        read_numbers(line.substr(0, line.find(kSep))),
        read_numbers(line.substr(line.find(kSep) + kSep.size())));
  }
  return cards;
}

uint64_t part1(const std::vector<Card> &cards) {
  uint64_t answ{0};
  for (const auto &card : cards) {
    answ += card.points;
  }
  return answ;
}

uint64_t part2(std::vector<Card> cards) {
  const uint64_t total = cards.size();
  for (uint64_t i = 0; i < total; ++i) {
    const auto &cur = cards[i];
    for (uint64_t j = i + 1; j <= std::min(i + cur.matching, total - 1); ++j) {
      cards[j].copies += cur.copies;
    }
  }
  uint64_t answ{0};
  for (const auto &card : cards) {
    answ += card.copies;
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input04.txt")) << std::endl;
  std::cout << part2(parse("input04.txt")) << std::endl;
  return 0;
}