#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
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
    std::stringstream ss(line);
    for (int n; ss >> n;) {
      numbers.emplace_back(n);
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
  return std::accumulate(
      cards.begin(), cards.end(), 0,
      [](auto acc, auto &card) { return acc + card.points; });
}

uint64_t part2(std::vector<Card> cards) {
  const uint64_t total = cards.size();
  uint64_t answ{cards.size()};
  for (uint64_t i = 0; i < total; ++i) {
    const auto &cur = cards[i];
    for (uint64_t j = i + 1; j <= std::min(i + cur.matching, total - 1); ++j) {
      cards[j].copies += cur.copies;
      answ += cur.copies;
    }
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input04.txt")) << std::endl;
  std::cout << part2(parse("input04.txt")) << std::endl;
  return 0;
}