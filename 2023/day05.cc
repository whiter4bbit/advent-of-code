#include <deque>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

struct Range {
  uint64_t lo{0};
  uint64_t hi{0};
  uint64_t offset_pos{0};
  uint64_t offset_neg{0};
  uint64_t map(uint64_t s) const {
    return s + offset_pos - offset_neg;
  }
};

void sort(std::vector<Range>& ranges) {
  std::sort(ranges.begin(), ranges.end(), [](const auto& a, const auto& b) {
    return a.lo < b.lo;
  });
}

struct Map {
  std::vector<Range> ranges;
  uint64_t map(uint64_t y) const {
    for (const auto &range : ranges) {
      if (y >= range.lo && y <= range.hi) {
        return range.map(y);
      }
    }
    return y;
  }
  std::vector<Range> map(const std::vector<Range> &seeds) const {
    std::deque<Range> seedRanges(seeds.begin(), seeds.end());
    std::deque<Range> mapRanges(ranges.begin(), ranges.end());
    std::vector<Range> answ;
    while (!seedRanges.empty()) {
      auto &seed = seedRanges.front();
      if (mapRanges.empty()) {
        answ.emplace_back(seed);
        seedRanges.pop_front();
        continue;
      }
      auto &map = mapRanges.front();
      if (map.lo < seed.lo) {
        //   SSS
        // MMMM
        map.lo = seed.lo;
      } else if (map.hi < seed.lo) {
        //     SSS
        // MMMM
        mapRanges.pop_front();
      } else if (seed.hi < map.lo) {
        // SSSSS
        //       MMMMM
        answ.emplace_back(seed);
        seedRanges.pop_front();
      } else if (seed.lo == map.lo) {
        // SSSSS
        // MMM
        auto hi = std::min(seed.hi, map.hi);
        answ.emplace_back(Range{
            .lo = map.map(seed.lo),
            .hi = map.map(hi),
        });
        if (seed.hi > hi) {
          seed.lo = hi + 1;
        } else {
          seedRanges.pop_front();
        }
        if (map.hi > hi) {
          map.lo = hi + 1;
        } else {
          mapRanges.pop_front();
        }
      } else if (seed.lo < map.lo) {
        answ.emplace_back(Range{
            .lo = seed.lo,
            .hi = map.lo - 1,
        });
        seed.lo = map.lo;
      } else {
        CHECK(false);
      }
    }
    sort(answ);
    return answ;
  }
};

struct Problem {
  std::vector<uint64_t> seeds;
  std::vector<Map> maps;
};

Problem parse(std::string_view path) {
  Problem problem;
  std::ifstream s(path);
  {
    std::string line;
    CHECK(std::getline(s, line));
    std::stringstream ss(line);

    std::string head;
    ss >> head;
    CHECK(head == "seeds:");

    for (uint64_t u; ss >> u;) {
      problem.seeds.emplace_back(u);
    }
  }
  for (std::string line; std::getline(s, line);) {
    if (line.find(" map:") != std::string::npos) {
      problem.maps.emplace_back();
    } else if (!line.empty()) {
      auto &map = problem.maps.back();
      uint64_t dst, src, n;
      std::stringstream ss(line);
      ss >> dst >> src >> n;
      map.ranges.emplace_back(Range{
          .lo = src,
          .hi = src + n - 1,
          .offset_pos = (dst > src) ? (dst - src) : 0,
          .offset_neg = (dst < src) ? (src - dst) : 0,
      });
    }
  }
  for (auto &map : problem.maps) {
    sort(map.ranges);
  }
  return problem;
}

uint64_t part1(const Problem &problem) {
  uint64_t answ{std::numeric_limits<uint64_t>::max()};
  for (auto s : problem.seeds) {
    for (const auto &map : problem.maps) {
      s = map.map(s);
    }
    answ = std::min(answ, s);
  }
  return answ;
}

uint64_t part2(const Problem &problem) {
  std::vector<Range> seeds(problem.seeds.size() / 2);
  for (int i = 0; i < problem.seeds.size(); i += 2) {
    seeds[i / 2].lo = problem.seeds[i];
    seeds[i / 2].hi = problem.seeds[i] + problem.seeds[i + 1] - 1;
  }
  sort(seeds);
  for (const auto &map : problem.maps) {
    seeds = map.map(seeds);
  }
  return seeds.front().lo;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input05-test.txt")) << std::endl;
  std::cout << part1(parse("input05.txt")) << std::endl;
  std::cout << part2(parse("input05-test.txt")) << std::endl;
  std::cout << part2(parse("input05.txt")) << std::endl;
  return 0;
}