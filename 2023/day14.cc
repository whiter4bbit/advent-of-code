#include <fstream>
#include <iostream>

using Pos = std::pair<int, int>;
using Platform = std::vector<std::string>;

Platform parse(std::string_view path) {
  std::ifstream s(path);
  Platform platform;
  for (std::string line; std::getline(s, line);) {
    platform.emplace_back(line);
  }
  return platform;
}

enum class Dir {
  N,
  W,
  S,
  E,
};

const std::unordered_map<Dir, std::pair<int, int>> kCornerOffset{
    {Dir::N, {0, 1}},
    {Dir::W, {1, 0}},
    {Dir::S, {0, -1}},
    {Dir::E, {-1, 0}},
};

const std::unordered_map<Dir, Pos> kSearchOffset{
    {Dir::N, {1, 0}},
    {Dir::S, {-1, 0}},
    {Dir::W, {0, 1}},
    {Dir::E, {0, -1}},
};

void tilt(Platform &platform, Dir dir) {
  const auto rows = platform.size();
  const auto cols = platform.front().size();

  Pos corner_pos{(dir == Dir::N || dir == Dir::W) ? 0 : rows - 1,
                 (dir == Dir::N || dir == Dir::W) ? 0 : cols - 1};

  auto within_bounds = [&](const auto &pos) {
    return pos.first >= 0 && pos.first < rows && pos.second >= 0 &&
           pos.second < cols;
  };

  auto step = [&](auto &pos, const auto &offset) {
    pos.first += offset.first;
    pos.second += offset.second;
  };

  for (; within_bounds(corner_pos); step(corner_pos, kCornerOffset.at(dir))) {
    for (auto land_pos = corner_pos; within_bounds(land_pos);
         step(land_pos, kSearchOffset.at(dir))) {
      if (platform[land_pos.first][land_pos.second] == '#' ||
          platform[land_pos.first][land_pos.second] == 'O') {
        continue;
      }

      std::optional<Pos> rock_pos;
      auto search_pos = land_pos;
      for (step(search_pos, kSearchOffset.at(dir)); within_bounds(search_pos);
           step(search_pos, kSearchOffset.at(dir))) {
        if (platform[search_pos.first][search_pos.second] == '#') {
          break;
        } else if (platform[search_pos.first][search_pos.second] == 'O') {
          rock_pos = search_pos;
          break;
        }
      }

      if (rock_pos) {
        platform[rock_pos->first][rock_pos->second] = '.';
        platform[land_pos.first][land_pos.second] = 'O';
      }
    }
  }
}

void show(const Platform &p) {
  std::cout << std::endl;
  for (const auto &row : p) {
    std::cout << row << std::endl;
  }
  std::cout << std::endl;
}

uint64_t compute_load(const Platform& platform) {
  const auto rows = platform.size();
  const auto cols = platform.front().size();
  uint64_t load{0};
  for (size_t row{0}; row < rows; ++row) {
    for (size_t col{0}; col < cols; ++col) {
      if (platform[row][col] == 'O') {
        load += (rows - row);
      }
    }
  }
  return load;
}

uint64_t part1(Platform platform) {
  tilt(platform, Dir::N);
  return compute_load(platform);
}

struct PlatformHash {
  size_t operator()(const Platform &p) const {
    size_t h{0};
    for (const auto &s : p) {
      h = h ^ (std::hash<std::string>{}(s) << 1LL);
    }
    return h;
  }
};

uint64_t part2(Platform platform) {
  auto cycle = [&]() {
    tilt(platform, Dir::N);
    tilt(platform, Dir::W);
    tilt(platform, Dir::S);
    tilt(platform, Dir::E);
  };
  std::unordered_map<Platform, int32_t, PlatformHash> seen;
  uint32_t count{1000000000};
  for (int32_t i{1};; ++i) {
    cycle();
    auto it = seen.find(platform);
    if (it != seen.end()) {
      count = (count - it->second) % (i - it->second);
      for (int32_t j{0}; j < count; ++j) {
        cycle();
      }
      break;
    }
    seen.emplace(platform, i);
  }
  return compute_load(platform);
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input14-test.txt")) << std::endl;
  std::cout << part1(parse("input14.txt")) << std::endl;
  std::cout << part2(parse("input14-test.txt")) << std::endl;
  std::cout << part2(parse("input14.txt")) << std::endl;
  return 0;
}