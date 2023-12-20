#include <fstream>
#include <iostream>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

struct Record {
  std::string condition;
  std::vector<int> damaged;
};

namespace std {
ostream &operator<<(ostream &os, const std::vector<int> &v) {
  for (int i = 0; i < v.size(); ++i) {
    if (i) {
      os << ",";
    }
    os << v[i];
  }
  return os;
}
}; // namespace std

std::vector<Record> parse(std::string_view path) {
  std::vector<Record> records;
  std::ifstream s(path);
  for (std::string left, right; s >> left >> right;) {
    auto &record = records.emplace_back(Record{
        .condition = left,
    });
    for (size_t offset{0}; offset < right.size();) {
      auto pos = right.find(',', offset);
      if (pos == std::string::npos) {
        pos = right.size();
      }
      record.damaged.emplace_back(
          std::atoi(right.substr(offset, pos - offset).c_str()));
      offset = pos + 1;
    }
  }
  return records;
}

struct PairHash {
  std::size_t operator()(const std::pair<int, int> &p) const {
    auto h1 = std::hash<int>{}(p.first);
    auto h2 = std::hash<int>{}(p.second);
    return h1 ^ (h2 << 1);
  }
};

struct Backtrack {
  uint64_t backtrack(int i, int c) {
    if (i < 0) {
      return c == -1;
    } else if (condition[i] == '.') {
      return backtrack(i - 1, c);
    } else if (damaged.empty()) {
      return (condition[i] == '?') ? backtrack(i - 1, c) : 0;
    }
    if (memo.find({i, c}) != memo.end()) {
      return memo.at({i, c});
    }
    CHECK(condition[i] == '?' || condition[i] == '#');
    uint64_t answ{0};
    if (condition[i] == '?') {
      answ += backtrack(i - 1, c);
    }
    int lo = i - damaged[c] + 1;
    if (lo < 0 || (lo > 0 && condition[lo - 1] == '#')) {
      memo[{i, c}] = answ;
      return answ;
    }
    for (int j{lo}; j <= i; ++j) {
      if (condition[j] != '#' && condition[j] != '?') {
        memo[{i, c}] = answ;
        return answ;
      }
    }
    memo[{i, c}] = backtrack(lo - 2, c - 1) + answ;
    return memo[{i, c}];
  }
  uint64_t backtrack() {
    return backtrack(static_cast<int>(condition.size()) - 1,
                     static_cast<int>(damaged.size()) - 1);
  }
  std::string condition;
  std::vector<int> damaged;
  std::unordered_map<std::pair<int, int>, uint64_t, PairHash> memo;
};

uint64_t count_arrangements(const std::vector<Record> &records,
                            bool print = false) {
  uint64_t answ{0};
  for (const auto &r : records) {
    Backtrack backtrack{
        .condition = r.condition,
        .damaged = r.damaged,
    };
    auto arr = backtrack.backtrack();
    if (print) {
      std::cout << "[" << r.condition.size() << "] " << arr << std::endl;
    }
    answ += arr;
  }
  return answ;
}

std::vector<Record> explode(const std::vector<Record> &records) {
  std::vector<Record> exploded;
  for (const auto &record : records) {
    auto &e = exploded.emplace_back();
    for (int i = 0; i < 5; ++i) {
      if (i > 0) {
        e.condition += "?";
      }
      e.condition += record.condition;
      e.damaged.insert(e.damaged.end(), record.damaged.cbegin(),
                       record.damaged.cend());
    }
  }
  return exploded;
}

int main(int argc, char **argv) {
  std::cout << count_arrangements(parse("input12-test.txt")) << std::endl;
  std::cout << count_arrangements(parse("input12.txt")) << std::endl;
  std::cout << count_arrangements(explode(parse("input12-test.txt")))
            << std::endl;
  std::cout << count_arrangements(explode(parse("input12.txt"))) << std::endl;
  return 0;
}