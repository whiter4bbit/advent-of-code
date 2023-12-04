#include <fstream>
#include <iostream>
#include <vector>

using Grid = std::vector<std::string>;

Grid read_grid(const std::string &path) {
  Grid lines;
  std::ifstream s(path);
  for (std::string line; std::getline(s, line);) {
    lines.emplace_back(line);
  }
  return lines;
}

struct Number {
  int loR{0}, hiR{0}, loC{0}, hiC{0};
  uint64_t v{0};
};

std::vector<Number> get_numbers(const Grid &grid) {
  std::vector<Number> numbers;
  for (int r{0}; r < grid.size(); ++r) {
    const auto &row = grid[r];
    for (int c{0}; c < row.size(); ++c) {
      if (!std::isdigit(row[c])) {
        continue;
      }
      auto &num = numbers.emplace_back(Number{
          .loR = r - 1,
          .hiR = r + 1,
          .loC = c - 1,
      });
      while (std::isdigit(row[c])) {
        num.v = num.v * 10 + (row[c++] - '0');
      }
      num.hiC = c;
    }
  }
  return numbers;
}

uint64_t part1(const Grid &grid) {
  auto is_symbol = [&](auto r, auto c) {
    if (r < 0 || r >= grid.size() || c < 0 || c >= grid[r].size()) {
      return false;
    }
    auto v = grid.at(r).at(c);
    return v != '.' && (v < '0' || v > '9');
  };
  uint64_t answ{0};
  for (const auto &num : get_numbers(grid)) {
    bool adjacent{false};
    for (int r{num.loR}; r <= num.hiR; ++r) {
      for (int c{num.loC}; c <= num.hiC; ++c) {
        adjacent |= is_symbol(r, c);
      }
    }
    if (adjacent) {
      answ += num.v;
    }
  }
  return answ;
}

uint64_t part2(const Grid &grid) {
  struct Gear {
    uint8_t nums{0};
    uint64_t ratio{1};
  };
  using GearPtr = std::unique_ptr<Gear>;
  const auto rows = grid.size();
  const auto cols = grid.front().size();  
  std::vector<std::vector<GearPtr>> gears(rows);
  for (int r{0}; r < rows; ++r) {
    const auto &row = grid[r];
    gears.at(r).resize(cols);
    for (int c{0}; c < cols; ++c) {
      if (row[c] == '*') {
        gears.at(r)[c] = std::make_unique<Gear>();
      }
    }
  }
  for (const auto &num : get_numbers(grid)) {
    for (int r{num.loR}; r <= num.hiR; ++r) {
      for (int c{num.loC}; c <= num.hiC; ++c) {
        if (r >= 0 && r < rows && c >= 0 && c < cols &&
            gears.at(r).at(c) != nullptr) {
          gears.at(r).at(c)->nums++;
          gears.at(r).at(c)->ratio *= num.v;
        }
      }
    }
  }
  uint64_t answ{0};
  for (const auto &row : gears) {
    for (const auto &col : row) {
      if (col != nullptr && col->nums == 2) {
        answ += col->ratio;
      }
    }
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << part1(read_grid("input03.txt")) << std::endl;
  std::cout << part2(read_grid("input03.txt")) << std::endl;
  return 0;
}