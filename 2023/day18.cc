#include <algorithm>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <unordered_set>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

using Pos = std::pair<int, int>;

struct PosHash {
  std::size_t operator()(const Pos &p) const {
    auto h1 = std::hash<int>{}(p.first);
    auto h2 = std::hash<int>{}(p.second);
    return h1 ^ (h2 << 1);
  }
};

enum class Dir { R, D, U, L };

Pos move_to(const Pos &p, Dir dir, int step = 1) {
  switch (dir) {
  case Dir::U:
    return Pos{p.first - step, p.second};
  case Dir::D:
    return Pos{p.first + step, p.second};
  case Dir::L:
    return Pos{p.first, p.second - step};
  case Dir::R:
    return Pos{p.first, p.second + step};
  }
}

struct Step {
  Dir dir;
  uint32_t times;
  uint32_t times_color;
  Dir dir_color;
};

std::vector<Step> parse(std::string_view path) {
  std::vector<Step> steps;
  std::ifstream s(path);
  for (std::string a, b, c; s >> a >> b >> c;) {
    Dir dir;
    if (a == "L") {
      dir = Dir::L;
    } else if (a == "R") {
      dir = Dir::R;
    } else if (a == "D") {
      dir = Dir::D;
    } else if (a == "U") {
      dir = Dir::U;
    } else {
      CHECK(false);
    }
    auto &step = steps.emplace_back(Step{
        .dir = dir,
        .times = static_cast<uint32_t>(std::stoi(b)),
        .times_color =
            static_cast<uint32_t>(std::stoi(c.substr(2, 5), nullptr, 16)),
    });
    if (c[7] == '0') {
      step.dir_color = Dir::R;
    } else if (c[7] == '1') {
      step.dir_color = Dir::D;
    } else if (c[7] == '2') {
      step.dir_color = Dir::L;
    } else if (c[7] == '3') {
      step.dir_color = Dir::U;
    } else {
      CHECK(false);
    }
  }
  return steps;
}

uint32_t part1(const std::vector<Step> &steps) {
  Pos pos{0, 0};
  std::unordered_map<Pos, char, PosHash> trench; //{{pos, '#'}};
  int min_row{0}, max_row{0}, min_col{0}, max_col{0};
  auto draw = [&]() {
    std::cout << "\n";
    for (int r{min_row}; r <= max_row; ++r) {
      std::string row;
      for (int c{min_col}; c <= max_col; ++c) {
        Pos p{r, c};
        if (trench.find(p) == trench.end()) {
          row += ".";
        } else {
          row += trench[p];
        }
      }
      std::cout << row << "\n";
    }
    std::cout << "\n";
  };
  for (const auto &step : steps) {
    for (uint32_t i{0}; i < step.times; ++i) {
      pos = move_to(pos, step.dir);
      min_row = std::min(min_row, pos.first - 1);
      max_row = std::max(max_row, pos.first + 1);
      min_col = std::min(min_col, pos.second - 1);
      max_col = std::max(max_col, pos.second + 1);
      trench[pos] = '#';
    }
  }
  trench[{min_row, min_col}] = 'O';
  std::deque<Pos> queue{Pos{min_row, min_col}};
  while (!queue.empty()) {
    auto pos = queue.front();
    queue.pop_front();

    for (auto dir : {Dir::L, Dir::R, Dir::U, Dir::D}) {
      auto next_pos = move_to(pos, dir);
      if (next_pos.first >= min_row && next_pos.first <= max_row &&
          next_pos.second >= min_col && next_pos.second <= max_col &&
          trench.find(next_pos) == trench.end()) {
        trench[next_pos] = 'O';
        queue.push_back(next_pos);
      }
    }
  }
  uint64_t blocks{0};
  for (int r{min_row}; r <= max_row; ++r) {
    for (int c{min_col}; c <= max_col; ++c) {
      Pos p{r, c};
      if (trench.find(p) == trench.end() || trench.at(p) == '#') {
        ++blocks;
      }
    }
  }
  // draw();
  return blocks;
}

uint64_t part2(const std::vector<Step> &steps) {
  enum class Mark {
    LAND,
    TRENCH,
    OUT,
  };
  struct Cell {
    uint64_t size{0};
    Mark mark{Mark::LAND};
  };
  std::vector<std::vector<Cell>> map;
  {
    std::vector<int> c_cols;
    std::vector<int> c_rows;

    Pos end{0, 0};
    for (const auto &step : steps) {
      auto start = move_to(end, step.dir_color, 1);
      end = move_to(start, step.dir_color, step.times_color - 1);

      c_cols.emplace_back(std::min(start.second, end.second));
      c_cols.emplace_back(std::max(start.second, end.second) + 1);
      c_rows.emplace_back(std::min(start.first, end.first));
      c_rows.emplace_back(std::max(start.first, end.first) + 1);
    }

    std::sort(c_cols.begin(), c_cols.end());
    c_cols.erase(std::unique(c_cols.begin(), c_cols.end()), c_cols.end());

    std::sort(c_rows.begin(), c_rows.end());
    c_rows.erase(std::unique(c_rows.begin(), c_rows.end()), c_rows.end());

    for (int r{0}; r < c_rows.size(); ++r) {
      auto &row = map.emplace_back();
      for (int c{0}; c < c_cols.size(); ++c) {
        const uint64_t heigth =
            (r == c_rows.size() - 1) ? 1 : (c_rows[r + 1] - c_rows[r]);
        const uint64_t width =
            (c == c_cols.size() - 1) ? 1 : (c_cols[c + 1] - c_cols[c]);
        auto &cell = row.emplace_back();
        cell.size = (heigth * width);

        Pos end{0, 0};
        for (const auto &step : steps) {
          auto start = move_to(end, step.dir_color, 1);
          end = move_to(start, step.dir_color, step.times_color - 1);

          auto s_row = std::min(start.first, end.first);
          auto e_row = std::max(start.first, end.first);
          auto s_col = std::min(start.second, end.second);
          auto e_col = std::max(start.second, end.second);

          if (c_rows[r] >= s_row && c_rows[r] <= e_row && c_cols[c] >= s_col &&
              c_cols[c] <= e_col) {
            cell.mark = Mark::TRENCH;
          }
        }
      }
    }
  }
  auto show = [&]() {
    std::cout << "\n";
    for (const auto &row : map) {
      for (const auto &cell : row) {
        if (cell.mark == Mark::TRENCH) {
          std::cout << '#';
        } else if (cell.mark == Mark::OUT) {
          std::cout << 'O';
        } else {
          std::cout << '.';
        }
      }
      std::cout << "\n";
    }
  };
  const auto rows = map.size();
  const auto cols = map.front().size();
  std::deque<Pos> queue;
  for (int r{0}; r < rows; ++r) {
    if (map[r][0].mark == Mark::LAND) {
      queue.emplace_back(r, 0);
    }
    if (map[r][cols - 1].mark == Mark::LAND) {
      queue.emplace_back(r, cols - 1);
    }
  }
  for (int c{0}; c < cols; ++c) {
    if (map[0][c].mark == Mark::LAND) {
      queue.emplace_back(0, c);
    }
    if (map[rows - 1][c].mark == Mark::LAND) {
      queue.emplace_back(rows - 1, c);
    }
  }
  while (!queue.empty()) {
    auto pos = queue.front();
    queue.pop_front();
    map[pos.first][pos.second].mark = Mark::OUT;

    for (auto dir : {Dir::U, Dir::D, Dir::L, Dir::R}) {
      auto [r, c] = move_to(pos, dir);
      if (r >= 0 && r < rows && c >= 0 && c < cols &&
          map[r][c].mark == Mark::LAND) {
        map[r][c].mark = Mark::OUT;
        queue.emplace_back(r, c);
      }
    }
  }

  // show();
  uint64_t blocks{0};
  for (const auto &row : map) {
    for (const auto &cell : row) {
      if (cell.mark == Mark::LAND || cell.mark == Mark::TRENCH) {
        blocks += cell.size;
      }
    }
  }
  return blocks;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input18-test.txt")) << std::endl;
  std::cout << part1(parse("input18.txt")) << std::endl;
  std::cout << part2(parse("input18-test.txt")) << std::endl;
  std::cout << part2(parse("input18.txt")) << std::endl;
  return 0;
}
