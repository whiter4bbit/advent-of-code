#include <deque>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

struct Race {
  uint64_t time{0};
  uint64_t record{0};
};

Race compact(const std::vector<Race> &races) {
  Race compacted {
    .time = races.front().time,
    .record = races.front().record,
  };
  auto append = [](uint64_t& d, uint64_t v) {
    auto s = std::to_string(v);
    for (int j = 0; j < s.size(); ++j) {
      d *= 10;
    }
    d += v;
  };
  for (int i = 1; i < races.size(); ++i) {
    append(compacted.time, races[i].time);
    append(compacted.record, races[i].record);
  }
  return compacted; 
}

std::vector<Race> parse(std::string_view path) {
  std::ifstream s(path);
  std::string line, header;
  std::vector<Race> races;
  {
    CHECK(std::getline(s, line));
    std::stringstream ss(line);
    ss >> header;
    for (uint64_t time; ss >> time;) {
      races.emplace_back(Race{
          .time = time,
      });
    }
  }
  {
    CHECK(std::getline(s, line));
    std::stringstream ss(line);
    ss >> header;
    for (uint64_t i = 0, record; i < races.size(); ++i) {
      CHECK(ss >> record);
      races[i].record = record;
    }
  }
  return races;
}

uint64_t ways_to_win(const Race &race) {
  uint64_t ways{0};
  for (uint64_t h{1}; h < race.time; ++h) {
    if ((race.time - h) * h > race.record) {
      ++ways;
    }
  }
  return ways;
}

uint64_t part1(const std::vector<Race> &races) {
  uint64_t answ{1};
  for (const auto &race : races) {
    answ *= ways_to_win(race);
  }
  return answ;
}

uint64_t part2(const Race &race) {
  return ways_to_win(race);
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input06-test.txt")) << std::endl;
  std::cout << part1(parse("input06.txt")) << std::endl;
  std::cout << part2(compact(parse("input06-test.txt"))) << std::endl;
  std::cout << part2(compact(parse("input06.txt"))) << std::endl;
  return 0;
}
