#include <deque>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unordered_set>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

enum class Dir { N, W, S, E, NE, NW, SE, SW };

const std::unordered_map<Dir, std::pair<int, int>> kDirs{
    {Dir::N, {-1, 0}}, {Dir::W, {0, -1}},  {Dir::S, {1, 0}},
    {Dir::E, {0, 1}},  {Dir::NE, {-1, 1}}, {Dir::NW, {-1, -1}},
    {Dir::SE, {1, 1}}, {Dir::SW, {1, -1}},
};

struct Pipe {
  Pipe(int r, int c, char ch) : r(r), c(c), ch(ch) {}
  std::vector<Pipe *> next;
  int r, c;
  char ch;
  void append(Pipe *n) {
    for (auto p : next) {
      if (p == n) {
        return;
      }
    }
    next.emplace_back(n);
  }
  Pipe *follow(Dir dir) {
    int follow_r = r + kDirs.at(dir).first;
    int follow_c = c + kDirs.at(dir).second;
    for (auto p : next) {
      if (p != nullptr && p->r == follow_r && p->c == follow_c) {
        return p;
      }
    }
    CHECK(false);
    return nullptr;
  }
};

struct Grid {
  std::vector<Pipe *> pipes;
  int rows;
  int cols;
  Pipe *animal;
  ~Grid() {
    for (auto p : pipes) {
      delete p;
    }
  }
};

const std::unordered_map<Dir, Dir> kOpposite{
    {Dir::N, Dir::S},
    {Dir::S, Dir::N},
    {Dir::E, Dir::W},
    {Dir::W, Dir::E},
};

const std::unordered_map<char, std::vector<Dir>> kPipes{
    {'|', {Dir::N, Dir::S}}, {'-', {Dir::E, Dir::W}}, {'L', {Dir::N, Dir::E}},
    {'J', {Dir::N, Dir::W}}, {'7', {Dir::S, Dir::W}}, {'F', {Dir::S, Dir::E}},
};

Grid parse(std::string_view path) {
  std::vector<std::string> text_grid;
  std::ifstream s(path);
  for (std::string line; std::getline(s, line);) {
    text_grid.emplace_back("." + line + ".");
  }
  const int cols = text_grid.front().size();
  text_grid.insert(text_grid.begin(), std::string(cols, '.'));
  text_grid.emplace_back(std::string(cols, '.'));

  const int rows = text_grid.size();

  Grid grid{
      .rows = rows,
      .cols = cols,
  };
  std::vector<std::vector<Pipe *>> pipes;
  for (int r{0}; r < rows; ++r) {
    auto &p_row = pipes.emplace_back();
    for (int c{0}; c < cols; ++c) {
      const auto cell = text_grid[r][c];
      auto pipe =
          p_row.emplace_back((cell != '.') ? new Pipe(r, c, cell) : nullptr);
      if (pipe != nullptr) {
        grid.pipes.emplace_back(pipe);
      }
    }
  }

  for (int r{1}; r < rows - 1; ++r) {
    for (int c{1}; c < cols - 1; ++c) {
      auto pipe = pipes.at(r).at(c);
      auto cell = text_grid.at(r).at(c);
      if (cell != '.' && cell != 'S') {
        for (auto dir : kPipes.at(cell)) {
          auto [r_off, c_off] = kDirs.at(dir);
          auto next_cell = text_grid.at(r + r_off).at(c + c_off);
          auto next_pipe = pipes.at(r + r_off).at(c + c_off);
          if ((kPipes.find(next_cell) != kPipes.end() &&
               (kPipes.at(next_cell).front() == kOpposite.at(dir) ||
                kPipes.at(next_cell).back() == kOpposite.at(dir))) ||
              next_cell == 'S') {
            pipe->append(next_pipe);
            next_pipe->append(pipe);
          }
        }
      }
      if (cell == 'S') {
        grid.animal = pipe;
      }
    }
  }
  {
    std::unordered_set<Dir> next;
    auto s = grid.animal;
    for (auto n : s->next) {
      for (auto [dir, offset] : kDirs) {
        std::pair<int, int> n_offset{n->r - s->r, n->c - s->c};
        if (offset == n_offset) {
          next.insert(dir);
        }
      }
    }

    for (auto [p, dirs] : kPipes) {
      auto found = std::all_of(dirs.begin(), dirs.end(), [&](auto dir) {
        return next.find(dir) != next.end();
      });
      if (found) {
        s->ch = p;
      }
    }
    CHECK(s->ch != 'S');
  }
  return grid;
}

std::vector<Pipe *> find_loop(Grid &grid) {
  std::vector<Pipe *> loop;
  for (auto cur = grid.animal, prev = grid.animal;;) {
    loop.emplace_back(cur);
    if ((prev != grid.animal) &&
        (cur->next.front() == grid.animal || cur->next.back() == grid.animal)) {
      break;
    }
    if (cur->next.front() != prev) {
      prev = cur;
      cur = cur->next.front();
    } else if (cur->next.back() != prev) {
      prev = cur;
      cur = cur->next.back();
    } else {
      CHECK(false);
    }
  }
  return loop;
}

uint64_t part1(Grid grid) {
  uint64_t len = find_loop(grid).size() - 1;
  return (len % 2) ? (len / 2 + 1) : (len / 2);
}

const std::unordered_map<char, std::unordered_map<Dir, Dir>> kNext{
    {'F', {{Dir::N, Dir::E}, {Dir::W, Dir::S}}},
    {'L', {{Dir::S, Dir::E}, {Dir::W, Dir::N}}},
    {'J', {{Dir::E, Dir::N}, {Dir::S, Dir::W}}},
    {'7', {{Dir::E, Dir::S}, {Dir::N, Dir::W}}},
    {'-', {{Dir::E, Dir::E}, {Dir::W, Dir::W}}},
    {'|', {{Dir::S, Dir::S}, {Dir::N, Dir::N}}},
};

const std::unordered_map<char, std::unordered_map<Dir, std::vector<Dir>>> kInTiles{
  {
    'F',
    {
      {Dir::N, {Dir::N, Dir::NW, Dir::W}},
      {Dir::W, {Dir::SE}},
    }
  },
  {
    'L',
    {
      {Dir::S, {Dir::NE}},
      {Dir::W, {Dir::S, Dir::W, Dir::SW}},
    }
  },
  {
    'J',
    {
      {Dir::E, {Dir::NW}},
      {Dir::S, {Dir::E, Dir::S, Dir::SE}},
    }
  },
  {
    '7',
    {
      {Dir::N, {Dir::SW}},
      {Dir::E, {Dir::N, Dir::E, Dir::NE}},
    }
  },
  {
    '|',
    {
      {Dir::S, {Dir::E}},
      {Dir::N, {Dir::W}},
    }
  },
  {
    '-',
    {
      {Dir::E, {Dir::N}},
      {Dir::W, {Dir::S}},
    }
  }
};

uint64_t part2(Grid grid) {
  const int rows = grid.rows;
  const int cols = grid.cols;
  std::vector<std::vector<Pipe *>> map(rows, std::vector<Pipe *>(cols));
  for (auto pipe : find_loop(grid)) {
    map[pipe->r][pipe->c] = pipe;
  }
  auto show = [&]() {
    std::cout << "\n";
    for (int r{0}; r < rows; ++r) {
      std::string row;
      for (int c{0}; c < cols; ++c) {
        row += map[r][c] ? map[r][c]->ch : '.';
      }
      std::cout << row << "\n";
    }
    std::cout << "\n";
  };
  Pipe *pipe{nullptr};
  {
    for (auto &row : map) {
      int c{0};
      while (c < row.size() && row[c] == nullptr) {
        c++;
      }
      if (c < row.size() && row[c]->ch == '|') {
        pipe = row[c];
        break;
      }
    }
    CHECK(pipe);
  }
  std::deque<Pipe *> queue;
  Dir dir{Dir::S};
  for (int i{0}; i < grid.pipes.size(); ++i) {
    for (auto mark_dir : kInTiles.at(pipe->ch).at(dir)) {
      auto mark_row = pipe->r + kDirs.at(mark_dir).first;
      auto mark_col = pipe->c + kDirs.at(mark_dir).second;
      if (map[mark_row][mark_col] == nullptr) {
        map[mark_row][mark_col] = new Pipe(mark_row, mark_col, 'I');
        queue.push_back(map[mark_row][mark_col]);
      }
    }
    dir = kNext.at(pipe->ch).at(dir);
    pipe = pipe->follow(dir);
  }
  uint64_t count{0};
  while (!queue.empty()) {
    ++count;
    auto cell = queue.front();
    queue.pop_front();

    for (auto [_, offset] : kDirs) {
      auto [r_offset, c_offset] = offset;
      auto r = cell->r + r_offset;
      auto c = cell->c + c_offset;

      if (map[r][c] == nullptr) {
        map[r][c] = new Pipe(r, c, 'I');
        queue.push_back(map[r][c]);
      }
    }
  }
  // show();
  return count;
}

void draw_loop(const Grid &grid) {
  std::vector<std::string> plot(grid.rows, std::string(grid.cols, '.'));
  plot[grid.animal->r][grid.animal->c] = grid.animal->ch;
  for (auto pipe = grid.animal->next.front(), from = grid.animal;;) {
    plot[pipe->r][pipe->c] = pipe->ch;
    CHECK(pipe->next.size() == 2);
    if (from != grid.animal && (pipe->next.back() == grid.animal ||
                                pipe->next.front() == grid.animal)) {
      break;
    } else if (pipe->next.front() != from) {
      from = pipe;
      pipe = pipe->next.front();
    } else if (pipe->next.back() != from) {
      from = pipe;
      pipe = pipe->next.back();
    } else if (pipe->next.back() == grid.animal ||
               pipe->next.front() == grid.animal) {
      break;
    }
  }
  for (const auto &row : plot) {
    std::cout << row << std::endl;
  }
}

int main(int argc, char **argv) {
  std::cout << "P1T: " << part1(parse("input10-test.txt")) << std::endl;
  std::cout << "P1P: " << part1(parse("input10.txt")) << std::endl;
  std::cout << "P2T: " << part2(parse("input10-test2.txt")) << std::endl;
  std::cout << "P2P: " << part2(parse("input10.txt")) << std::endl;
  return 0;
}