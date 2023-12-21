#include <fstream>
#include <iostream>
#include <queue>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

using Pos = std::pair<int, int>;
using Map = std::vector<std::vector<uint64_t>>;

Map parse(std::string_view path) {
  std::ifstream s(path);
  Map map;
  for (std::string row; std::getline(s, row);) {
    auto &blocks = map.emplace_back();
    for (const auto &c : row) {
      blocks.emplace_back(c - '0');
    }
  }
  return map;
}

enum class Dir {
  N,
  S,
  W,
  E,
  U,
};

Pos move_to(const Pos &pos, Dir dir) {
  switch (dir) {
  case Dir::N:
    return Pos{
        pos.first - 1,
        pos.second,
    };
  case Dir::S:
    return Pos{
        pos.first + 1,
        pos.second,
    };
  case Dir::E:
    return Pos{
        pos.first,
        pos.second + 1,
    };
  case Dir::W:
    return Pos{
        pos.first,
        pos.second - 1,
    };
  default:
    CHECK(false);
  }
}

const std::unordered_map<Dir, std::vector<Dir>> kTurns{
    {Dir::N, {Dir::E, Dir::W}},
    {Dir::S, {Dir::E, Dir::W}},
    {Dir::E, {Dir::S, Dir::N}},
    {Dir::W, {Dir::S, Dir::N}},
    {Dir::U, {Dir::E, Dir::N, Dir::W, Dir::S}},
};

struct State {
  struct Hash {
    size_t operator()(const State &s) const {
      return std::hash<int>{}(s.pos.first) ^
             (std::hash<int>{}(s.pos.second) << 1) ^
             (std::hash<Dir>{}(s.dir) << 2);
    }
  };
  bool operator==(const State &o) const { return pos == o.pos && dir == o.dir; }
  std::pair<int, int> pos;
  Dir dir;
};

struct StateCost {
  bool operator<(const StateCost &o) const { return cost > o.cost; }
  State state;
  uint64_t cost{0};
};

uint64_t least_heat_loss(const Map &map, int min_steps, int max_steps) {
  const auto rows = map.size();
  const auto cols = map.front().size();
  const Pos kTarget{rows - 1, cols - 1};

  State seed{.pos = {0, 0}, .dir = Dir::U};
  std::unordered_map<State, uint64_t, State::Hash> costs{{seed, 0}};
  std::priority_queue<StateCost, std::vector<StateCost>> queue;
  queue.push(StateCost{
      .state = seed,
      .cost = 0,
  });

  while (!queue.empty()) {
    auto state_cost = queue.top();
    queue.pop();
    if (costs.at(state_cost.state) != state_cost.cost) {
      continue;
    }
    const auto &state = state_cost.state;
    if (state.pos == kTarget) {
      return state_cost.cost;
    }
    for (auto next_dir : kTurns.at(state.dir)) {
      uint64_t next_cost{state_cost.cost};
      auto next_pos = state.pos;
      for (int step{1}; step <= max_steps; ++step) {
        next_pos = move_to(next_pos, next_dir);
        if (next_pos.first < 0 || next_pos.first >= rows ||
            next_pos.second < 0 || next_pos.second >= cols) {
          break;
        }
        next_cost += map.at(next_pos.first).at(next_pos.second);
        if (step < min_steps) {
          continue;
        }
        State next_state{
            .pos = next_pos,
            .dir = next_dir,
        };
        if (costs.find(next_state) == costs.end() ||
            costs.at(next_state) > next_cost) {
          costs[next_state] = next_cost;
          queue.push(StateCost{
              .state = next_state,
              .cost = next_cost,
          });
        }
      }
    }
  }
  return std::numeric_limits<uint64_t>::max();
}

int main(int argc, char **argv) {
  std::cout << least_heat_loss(parse("input17-test.txt"), 1, 3) << std::endl;
  std::cout << least_heat_loss(parse("input17.txt"), 1, 3) << std::endl;
  std::cout << least_heat_loss(parse("input17-test.txt"), 4, 10) << std::endl;
  std::cout << least_heat_loss(parse("input17.txt"), 4, 10) << std::endl;
  return 0;
}
