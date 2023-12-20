#include <fstream>
#include <iostream>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

using Pattern = std::vector<std::string>;

std::vector<Pattern> parse(std::string_view path) {
  std::vector<Pattern> patterns(1);
  std::ifstream s(path);
  std::string line;
  while (std::getline(s, line)) {
    if (line.empty()) {
      patterns.emplace_back();
    } else {
      patterns.back().emplace_back(line);
    }
  }
  return patterns;
}

std::vector<size_t> find_vertical(const Pattern &pattern) {
  auto cols = pattern.front().size();
  auto col_match = [&](auto col, auto width) {
    for (const auto &row : pattern) {
      for (size_t w{0}; w < width; ++w) {
        if (row.at(col + w) != row.at(col - 1 - w)) {
          return false;
        }
      }
    }
    return true;
  };
  std::vector<size_t> lines;
  for (size_t col{1}; col < cols; ++col) {
    if (col_match(col, std::min(col, cols - col))) {
      lines.emplace_back(col);
    }
  }
  return lines;
}

std::vector<size_t> find_horizontal(const Pattern &pattern) {
  auto row_match = [&](auto row, auto height) {
    for (size_t h{0}; h < height; ++h) {
      if (pattern.at(row + h) != pattern.at(row - 1 - h)) {
        return false;
      }
    }
    return true;
  };
  std::vector<size_t> lines;
  auto rows = pattern.size();
  for (size_t row{1}; row < rows; ++row) {
    if (row_match(row, std::min(row, rows - row))) {
      lines.emplace_back(row);
    }
  }
  return lines;
}

std::vector<size_t> reflection_cost(const Pattern &p) {
  std::vector<size_t> res;
  auto v_lines = find_vertical(p);
  res.insert(res.begin(), v_lines.cbegin(), v_lines.cend());
  auto h_lines = find_horizontal(p);
  for (auto &line : h_lines) {
    line *= 100;
  }
  res.insert(res.begin(), h_lines.cbegin(), h_lines.cend());
  return res;
}

std::vector<size_t> relection_cost_flipped(const Pattern &pattern,
                                           const std::vector<size_t> &current) {
  const auto rows = pattern.size();
  const auto cols = pattern.front().size();
  for (size_t r{0}; r < rows; ++r) {
    for (size_t c{0}; c < cols; ++c) {
      auto f_pattern = pattern;
      f_pattern[r][c] = (f_pattern[r][c] == '#') ? '.' : '#';
      auto lines = reflection_cost(f_pattern);
      if (!lines.empty() && current != lines) {
        return lines;
      }
    }
  }
  return {};
}

uint64_t part1(const std::vector<Pattern> &patterns) {
  uint64_t answ{0};
  for (const auto &p : patterns) {
    auto lines = reflection_cost(p);
    CHECK(!lines.empty());
    answ += lines.front();
  }
  return answ;
}

uint64_t part2(const std::vector<Pattern> &patterns) {
  uint64_t answ{0};
  for (const auto &p : patterns) {
    auto current = reflection_cost(p);
    CHECK(current.size() == 1);
    auto flipped = relection_cost_flipped(p, current);
    CHECK(!flipped.empty());
    for (auto &v : flipped) {
      if (current.front() != v) {
        answ += v;
      }
    }
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input13-test.txt")) << std::endl;
  std::cout << part1(parse("input13.txt")) << std::endl;
  std::cout << part2(parse("input13-test.txt")) << std::endl;
  std::cout << part2(parse("input13.txt")) << std::endl;
  return 0;
}