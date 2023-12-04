#include <iostream>
#include <fstream>
#include <vector>

struct Revealed {
  uint32_t r{0}, g{0}, b{0};
};

using Game = std::vector<Revealed>;

#define CHECK(cond) \
  if(!(cond)) { throw std::runtime_error(#cond); };

std::vector<Game> parse(const std::string& path) {
  // Game 14: 4 green; 8 blue, 1 red, 2 green; 7 red, 2 green, 4 blue; 4 blue, 7 green; 7 blue, 2 green, 1 red; 7 blue, 5 red
  std::vector<Game> parsed;
  std::ifstream s(path);
  for (std::string line; std::getline(s, line);) {
    auto& game = parsed.emplace_back();
    
    line = line.substr(line.find(": ") + 2);
    while (!line.empty()) {
      std::string rev;
      if (auto sep = line.find("; "); sep != std::string::npos) {
        rev = line.substr(0, sep);
        line = line.substr(sep + 2);
      } else {
        rev = line;
        line.clear();
      }
      
      auto& revealed = game.emplace_back();
      while (!rev.empty()) {
        std::string count;
        if (auto sep = rev.find(", "); sep != std::string::npos) {
          count = rev.substr(0, sep);
          rev = rev.substr(sep + 2);
        } else {
          count = rev;
          rev.clear();
        }

        auto value = std::stoi(count.substr(0, count.find(' ')));
        if (count.find("red") != std::string::npos) {
          revealed.r = value;
        } else if (count.find("green") != std::string::npos) {
          revealed.g = value;
        } else {
          CHECK(count.find("blue") != std::string::npos);
          revealed.b = value;
        }
      }
    }
  }
  return parsed;
}

uint64_t part1(const std::vector<Game>& games, uint32_t maxR, uint32_t maxG, uint32_t maxB) {
  uint64_t answ{0};
  for (int i{0}; i < games.size(); ++i) {
    const auto& game = games[i];
    bool possible = std::all_of(game.begin(), game.end(), [&](const auto& reveal) {
      return reveal.r <= maxR && reveal.g <= maxG && reveal.b <= maxB;
    });
    if (possible) {
      answ += i + 1;
    }
  }
  return answ;
}

uint64_t part2(const std::vector<Game>& games) {
  uint64_t answ{0};
  for (const auto& game : games) {
    uint32_t minR{0};
    uint32_t minG{0};
    uint32_t minB{0};
    for (const auto& r : game) {
      minR = std::max(minR, r.r);
      minG = std::max(minG, r.g);
      minB = std::max(minB, r.b);
    }
    answ += minR * minG * minB;
  }
  return answ;
}

int main(int argc, char** argv) {
  std::cout << part1(parse("input02.txt"), 12, 13, 14) << std::endl;
  std::cout << part2(parse("input02.txt")) << std::endl;
  // for (const auto& game : parse("input02-test.txt")) {
  //   std::cout << "Game\n";
  //   for (const auto& r : game) {
  //     std::cout << "   {" << r.r << ", " << r.g << ", " << r.b << "}\n";
  //   }
  // }
  return 0;
}