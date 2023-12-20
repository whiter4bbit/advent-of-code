#include <fstream>
#include <iostream>
#include <numeric>

struct Galaxy {
  int r, c;
};

std::vector<Galaxy> parse(std::string_view path, size_t expansion_width) {
  std::vector<std::string> map;
  std::ifstream s(path);
  for (std::string line; std::getline(s, line);) {
    map.emplace_back(line);
  }
  std::vector<Galaxy> galaxies;
  const auto rows = map.size();
  const auto cols = map.front().size();
  std::vector<size_t> col_offset(cols, expansion_width);
  std::vector<size_t> row_offset(rows, expansion_width);
  for (int r{0}; r < rows; ++r) {
    for (int c{0}; c < cols; ++c) {
      if (map[r][c] == '#') {
        col_offset[c] = 0;
        row_offset[r] = 0;
        galaxies.emplace_back(Galaxy{
            .r = r,
            .c = c,
        });
      }
    }
  }
  std::partial_sum(col_offset.cbegin(), col_offset.cend(), col_offset.begin());
  std::partial_sum(row_offset.cbegin(), row_offset.cend(), row_offset.begin());
  for (auto &galaxy : galaxies) {
    galaxy.r += row_offset[galaxy.r];
    galaxy.c += col_offset[galaxy.c];
  }
  return galaxies;
}

uint64_t part1(const std::vector<Galaxy> &galaxies) {
  uint64_t answ{0};
  for (int i = 0; i < galaxies.size(); ++i) {
    for (int j = i + 1; j < galaxies.size(); ++j) {
      auto distance = std::abs(galaxies[i].r - galaxies[j].r) +
                      std::abs(galaxies[i].c - galaxies[j].c);
      answ += distance;                                          
    }
  }
  return answ;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input11-test.txt", 1)) << std::endl;
  std::cout << part1(parse("input11.txt", 1)) << std::endl;
  std::cout << part1(parse("input11-test.txt", 100 - 1)) << std::endl;
  std::cout << part1(parse("input11.txt", 1000000 - 1)) << std::endl;
  return 0;
}