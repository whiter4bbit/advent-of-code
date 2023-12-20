#include <fstream>
#include <iostream>
#include <regex>
#include <unordered_set>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

struct Next {
  std::string left;
  std::string right;
};

struct Problem {
  std::string nav;
  std::unordered_map<std::string, Next> map;
};

Problem parse(std::string_view path) {
  Problem problem;
  std::ifstream s(path);
  s >> problem.nav;
  const std::regex node_regex("(\\w{3}) = \\((\\w{3}), (\\w{3})\\)");
  std::smatch match;
  for (std::string line; std::getline(s, line);) {
    if (std::regex_match(line, match, node_regex)) {
      problem.map.emplace(match[1], Next{.left = match[2], .right = match[3]});
    }
  }
  return problem;
}

uint32_t part1(const Problem &problem) {
  std::string cur{"AAA"};
  uint32_t steps{0};
  for (int i{0}; cur != "ZZZ"; ++i, ++steps) {
    auto n = problem.nav[i % problem.nav.size()];
    cur = (n == 'L') ? problem.map.at(cur).left : problem.map.at(cur).right;
  }
  return steps;
}

uint64_t part2(const Problem &problem) {
  std::vector<std::string> cur;
  for (const auto &[n, _] : problem.map) {
    if (n.back() == 'A') {
      cur.emplace_back(n);
    }
  }
  auto find_cycle = [&](auto node) {
    std::unordered_map<std::string, uint64_t> seen;
    for (uint64_t i{0};;) {
      auto n = problem.nav[i % problem.nav.size()];
      node =
          (n == 'L') ? problem.map.at(node).left : problem.map.at(node).right;
      ++i;
      if (node.back() == 'Z') {
        if (seen.find(node) != seen.end()) {
          return std::make_pair(seen[node], i - seen[node]);
        }
        seen[node] = i;
      }
    }
    CHECK(false);
  };
  std::vector<std::pair<uint64_t, uint64_t>> cycles;
  for (const auto &n : cur) {
    cycles.emplace_back(find_cycle(n));
  }
  while (true) {
    auto same = std::all_of(cycles.begin(), cycles.end(), [&](const auto &c) {
      return c.first == cycles.front().first;
    });
    if (same) {
      auto answ = cycles.front().first;
      return cycles.front().first;
    }
    auto push = std::min_element(
        cycles.begin(), cycles.end(),
        [](const auto &a, const auto &b) { return a.first < b.first; });
    push->first += push->second;
  }
  CHECK(false);
}

int main(int argc, char **argv) {
  std::cout << "P1T:" << part1(parse("input08-test.txt")) << std::endl;
  std::cout << "P1P:" << part1(parse("input08.txt")) << std::endl;
  std::cout << "P1T:" << part2(parse("input08-test2.txt")) << std::endl;
  std::cout << "P1P:" << part2(parse("input08.txt")) << std::endl;
  return 0;
}