#include <fstream>
#include <iostream>
#include <numeric>
#include <sstream>
#include <vector>

std::vector<std::vector<int64_t>> parse(std::string_view path) {
  std::ifstream s(path);
  std::vector<std::vector<int64_t>> result;
  for (std::string line; std::getline(s, line);) {
    std::stringstream ss(line);
    auto &seq = result.emplace_back();
    for (int64_t x; ss >> x;) {
      seq.emplace_back(x);
    }
  }
  return result;
}

enum class Direction { FRONT = 0, BACK };

int64_t extrapolate(const std::vector<int64_t> &seq, Direction dir) {
  auto all_zeros = [](const auto &seq) {
    return std::all_of(seq.begin(), seq.end(),
                       [](const auto &v) { return v == 0; });
  };
  std::vector<std::vector<int64_t>> st{seq};
  while (st.back().size() > 1 && !all_zeros(st.back())) {
    auto &next = st.emplace_back();
    auto &back = st[st.size() - 2];
    for (int i = 1; i < back.size(); ++i) {
      next.emplace_back(back[i] - back[i - 1]);
    }
  }
  int64_t p{0};
  for (int i = st.size() - 2; i >= 0; --i) {
    if (dir == Direction::BACK) {
      p = st[i].back() + p;
    } else {
      p = st[i].front() - p;
    }
  }
  return p;
}

int64_t solve(const std::vector<std::vector<int64_t>> &seqs, Direction dir) {
  return std::accumulate(
      seqs.begin(), seqs.end(), 0,
      [&](auto x, const auto &seq) { return x + extrapolate(seq, dir); });
}

int main(int argc, char **argv) {
  std::cout << solve(parse("input09-test.txt"), Direction::BACK) << std::endl;
  std::cout << solve(parse("input09.txt"), Direction::BACK) << std::endl;
  std::cout << solve(parse("input09-test.txt"), Direction::FRONT) << std::endl;
  std::cout << solve(parse("input09.txt"), Direction::FRONT) << std::endl;
  return 0;
}