#include <algorithm>
#include <fstream>
#include <iostream>
#include <optional>
#include <vector>

#define CHECK(cond) \
  if(!(cond)) { throw std::runtime_error(#cond); };

template <typename M>
uint64_t solve(M match, const std::string &path = "input01.txt") {
  std::ifstream s(path);
  uint64_t answ{0};
  for (std::string line; std::getline(s, line);) {
    std::optional<int> first;
    std::optional<int> last;
    for (int i = 0; i < line.size(); i++) {
      if (auto m = match(line, i)) {
        if (!first) {
          first = *m;
        }
        last = *m;
      }
    }
    CHECK(first && last);
    answ += (*first) * 10 + (*last);
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << solve([](const auto& line, auto pos) -> std::optional<int> {
    if (line.at(pos) >= '0' && line.at(pos) <= '9') {
      return line.at(pos) - '0';
    }
    return std::nullopt;
  }, "input01.txt") << std::endl;
  std::cout << solve([](const auto& line, auto pos) -> std::optional<int> {
    static constexpr std::array<std::string_view, 9> kDigits{
      "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
    if (line.at(pos) >= '0' && line.at(pos) <= '9') {
      return line.at(pos) - '0';
    }
    for (int i = 0; i < kDigits.size(); ++i) {
      if (line.find(kDigits.at(i), pos) == pos) {
        return i + 1;
      }
    }
    return std::nullopt;
  }, "input01.txt") << std::endl;
  return 0;
}