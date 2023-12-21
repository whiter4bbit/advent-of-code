#include <deque>
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <unordered_set>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

using Tile = char;
using Pos = std::pair<int, int>;

enum class Dir {
  N,
  S,
  E,
  W,
};

Pos apply_dir(const Pos &cur, Dir dir) {
  switch (dir) {
  case Dir::N:
    return Pos{cur.first - 1, cur.second};
  case Dir::S:
    return Pos{cur.first + 1, cur.second};
  case Dir::E:
    return Pos{cur.first, cur.second + 1};
  case Dir::W:
    return Pos{cur.first, cur.second - 1};
  }
}

struct Beam {
  Pos pos;
  Dir dir;
  bool operator==(const Beam &other) const {
    return other.pos == pos && other.dir == dir;
  }
};

Beam apply_dir(Beam &b, Dir dir) {
  return Beam{
      .pos = apply_dir(b.pos, dir),
      .dir = dir,
  };
}

struct Contraption {
  struct PosHash {
    size_t operator()(const Pos &pos) const {
      return std::hash<int>{}(pos.first) ^ (std::hash<int>{}(pos.second) << 1);
    }
  };
  struct BeamHash {
    size_t operator()(const Beam &beam) const {
      return std::hash<Dir>{}(beam.dir) ^ (PosHash{}(beam.pos) << 1);
    }
  };
  using Tiles = std::unordered_map<Pos, Tile, PosHash>;
  size_t rows, cols;
  std::shared_ptr<Tiles> tiles;
  std::unordered_set<Beam, BeamHash> track;
  std::unordered_set<Pos, PosHash> energized;
};

namespace std {
ostream &operator<<(ostream &os, const Contraption &cont) {
  os << "\n";
  for (size_t r{0}; r < cont.rows; ++r) {
    std::string row;
    for (size_t c{0}; c < cont.cols; ++c) {
      row += cont.tiles->at({r, c});
    }
    os << row << "\n";
  }
  os << "\n";
  return os;
}
} // namespace std

Contraption parse(std::string_view path) {
  std::ifstream s(path);
  std::vector<std::string> grid;
  for (std::string line; std::getline(s, line);) {
    grid.emplace_back(line);
  }
  Contraption cont{
      .rows = grid.size(),
      .cols = grid.front().size(),
      .tiles = std::make_shared<Contraption::Tiles>(),
  };
  for (size_t r{0}; r < cont.rows; ++r) {
    for (size_t c{0}; c < cont.cols; ++c) {
      cont.tiles->emplace(std::make_tuple(r, c), grid[r][c]);
    }
  }
  return cont;
}

void process(std::deque<Beam> &queue, Contraption &contr) {
  auto beam = queue.front();
  queue.pop_front();
  if (contr.tiles->find(beam.pos) == contr.tiles->end() ||
      contr.track.find(beam) != contr.track.end()) {
    return;
  }
  contr.track.emplace(beam);
  contr.energized.emplace(beam.pos);

  auto tile = contr.tiles->at(beam.pos);
  if (tile == '.') {
    queue.emplace_back(apply_dir(beam, beam.dir));
  } else if (tile == '|') {
    if (beam.dir == Dir::N || beam.dir == Dir::S) {
      queue.emplace_back(apply_dir(beam, beam.dir));
    } else {
      queue.emplace_back(apply_dir(beam, Dir::N));
      queue.emplace_back(apply_dir(beam, Dir::S));
    }
  } else if (tile == '-') {
    if (beam.dir == Dir::E || beam.dir == Dir::W) {
      queue.emplace_back(apply_dir(beam, beam.dir));
    } else {
      queue.emplace_back(apply_dir(beam, Dir::E));
      queue.emplace_back(apply_dir(beam, Dir::W));
    }
  } else if (tile == '/') {
    static const std::unordered_map<Dir, Dir> kReflectRight{
        {Dir::E, Dir::N},
        {Dir::W, Dir::S},
        {Dir::N, Dir::E},
        {Dir::S, Dir::W},
    };
    queue.emplace_back(apply_dir(beam, kReflectRight.at(beam.dir)));
  } else if (tile == '\\') {
    static const std::unordered_map<Dir, Dir> kReflectLeft{
        {Dir::E, Dir::S},
        {Dir::W, Dir::N},
        {Dir::S, Dir::E},
        {Dir::N, Dir::W},
    };
    queue.emplace_back(apply_dir(beam, kReflectLeft.at(beam.dir)));
  } else {
    CHECK(tile == '?');
  }
}

size_t count_energized(Contraption contr, Beam seed) {
  std::deque<Beam> queue;
  queue.emplace_back(seed);
  while (!queue.empty()) {
    process(queue, contr);
  }
  return contr.energized.size();
}

uint32_t part1(Contraption contr) {
  return count_energized(contr, Beam{
                                    .pos = {0, 0},
                                    .dir = Dir::E,
                                });
}

uint32_t part2(Contraption contr) {
  std::vector<Beam> seeds;
  for (size_t c{0}; c < contr.cols; ++c) {
    seeds.emplace_back(Beam{
        .pos = Pos{0, c},
        .dir = Dir::S,
    });
    seeds.emplace_back(Beam{
        .pos = Pos{contr.rows - 1, c},
        .dir = Dir::N,
    });
  }
  for (size_t r{0}; r < contr.rows; ++r) {
    seeds.emplace_back(Beam{
        .pos = Pos{r, 0},
        .dir = Dir::E,
    });
    seeds.emplace_back(Beam{
        .pos = Pos{r, contr.cols - 1},
        .dir = Dir::W,
    });
  }
  size_t max_energy{0};
  for (const auto &seed : seeds) {
    max_energy = std::max(max_energy, count_energized(contr, seed));
  }
  return max_energy;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input16-test.txt")) << std::endl;
  std::cout << part1(parse("input16.txt")) << std::endl;
  std::cout << part2(parse("input16-test.txt")) << std::endl;
  std::cout << part2(parse("input16.txt")) << std::endl;
  return 0;
}