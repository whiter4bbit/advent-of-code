#include <fstream>
#include <iostream>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

size_t aoc_hash(const std::string &s) {
  size_t h{0};
  for (const auto &c : s) {
    h = ((h + c) * 17) % 256;
  }
  return h;
}

std::vector<std::string> parse(std::string_view path) {
  std::vector<std::string> sequence;
  std::fstream s(path);
  for (std::string step; std::getline(s, step, ',');) {
    sequence.emplace_back(step);
  }
  return sequence;
}

uint64_t part1(const std::vector<std::string> &sequence) {
  uint64_t h{0};
  for (const auto &step : sequence) {
    h += aoc_hash(step);
  }
  return h;
}

struct Lens {
  std::string label;
  int focal{0};
};

uint64_t part2(const std::vector<std::string> &sequence) {
  std::vector<std::vector<Lens>> boxes(256);
  for (const auto &step : sequence) {
    if (step.find('-') != std::string::npos) {
      auto label = step.substr(0, step.size() - 1);
      auto &box = boxes.at(aoc_hash(label));
      auto it = std::find_if(box.begin(), box.end(),
                             [&](auto &e) { return e.label == label; });
      if (it != box.end()) {
        box.erase(it);
      }
    } else {
      Lens lens {
        .label = step.substr(0, step.find('=')),
        .focal = std::stoi(step.substr(step.find('=') + 1)),
      };
      auto &box = boxes.at(aoc_hash(lens.label));
      auto it = std::find_if(box.begin(), box.end(),
                             [&](auto &e) { return e.label == lens.label; });
      if (it == box.end()) {
        box.emplace_back(lens);
      } else {
        it->focal = lens.focal;
      }
    }
  }
  uint64_t power{0};
  for (uint64_t b{0}; b < boxes.size(); ++b) {
    const auto& box = boxes.at(b);
    for (uint64_t s{0}; s < box.size(); ++s) {
      power += (b + 1) * (s + 1) * box.at(s).focal;
    }
  }
  return power;
}

int main(int argc, char **argv) {
  std::cout << part1(parse("input15-test.txt")) << std::endl;
  std::cout << part1(parse("input15.txt")) << std::endl;
  std::cout << part2(parse("input15-test.txt")) << std::endl;
  std::cout << part2(parse("input15.txt")) << std::endl;
  return 0;
}