#include <fmt/ranges.h>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>

#define CHECK(cond)                                                            \
  if (!(cond)) {                                                               \
    throw std::runtime_error(#cond);                                           \
  };

using Input = std::unordered_map<std::string, int>;

struct Expr {
  std::string var;
  char op{'?'};
  int target{0};
  std::string goto_workflow;

  bool eval(const Input &v) const {
    if (op == '?') {
      return true;
    } else if (op == '<') {
      return v.at(var) < target;
    } else if (op == '>') {
      return v.at(var) > target;
    }
    CHECK(false);
  }
};

struct Workflow {
  std::string name;
  std::vector<Expr> exprs;

  std::string eval(const Input &v) const {
    for (const auto &e : exprs) {
      if (e.eval(v)) {
        return e.goto_workflow;
      }
    }
    CHECK(false);
  }
};

auto format_as(const Expr &e) {
  return fmt::format("{}{}{}:{}", e.var, e.op, e.target, e.goto_workflow);
}

auto format_as(const Workflow &w) { return fmt::format("{}", w.exprs); }

struct Problem {
  std::unordered_map<std::string, Workflow> workflows;
  std::vector<Input> inputs;
};

auto format_as(const Problem &p) {
  return fmt::format("workflows = {}, inputs = {}", p.workflows, p.inputs);
}

Problem parse(std::string_view path) {
  Problem problem;
  std::ifstream s(path);
  for (std::string line; std::getline(s, line);) {
    if (line.find('{') == 0) {
      auto &input = problem.inputs.emplace_back();
      std::stringstream ss(line.substr(1, line.size() - 1));
      for (std::string expr; std::getline(ss, expr, ',');) {
        input[expr.substr(0, 1)] = std::stoi(expr.substr(2, expr.size() - 2));
      }
    } else if (line.find('{') != std::string::npos) {
      static const std::regex kLineRe("(\\w+)\\{(.*)\\,(\\w+)\\}$");
      std::smatch match;
      CHECK(std::regex_match(line, match, kLineRe));
      auto &workflow = problem.workflows[match[1]];
      workflow.name = match[1];

      std::string else_workflow = match[3];
      std::stringstream ss(match[2]);
      for (std::string cond; std::getline(ss, cond, ',');) {
        // m<1801:hdj
        static const std::regex kExprRe("(\\w)([\\<\\>\\=])(\\d+)\\:(\\w+)");
        CHECK(std::regex_match(cond, match, kExprRe));
        auto &expr = workflow.exprs.emplace_back();
        expr.var = match[1];
        expr.op = static_cast<std::string>(match[2])[0];
        expr.target = std::stoi(static_cast<std::string>(match[3]));
        expr.goto_workflow = match[4];
      }

      auto &else_expr = workflow.exprs.emplace_back();
      else_expr.var = '*';
      else_expr.goto_workflow = else_workflow;
    }
  }
  return problem;
}

std::string step(const Workflow &w, const Input &input,
                 const Problem &problem) {
  auto next_name = w.eval(input);
  if (next_name == "A" || next_name == "R") {
    return next_name;
  }
  return step(problem.workflows.at(next_name), input, problem);
}

uint64_t part1(const Problem &problem) {
  uint64_t answ{0};
  for (const auto &input : problem.inputs) {
    if (step(problem.workflows.at("in"), input, problem) == "A") {
      for (const auto &[_, v] : input) {
        answ += v;
      }
    }
  }
  return answ;
}

using Mask = std::vector<size_t>();

static constexpr size_t kWide{4000};

const std::unordered_map<std::string, size_t> kVarPos{
    {"x", 0}, {"m", kWide}, {"a", kWide * 2}, {"s", kWide * 3}};

// void evaluate(const Problem &p, const std::string &name, std::vector<Mask> &masks,
//           Mask mask) {
//   if (name == "A") {
//     res.emplace_back(cur);
//   } else if (name != "R") {
//     for (const Expr &expr : p.workflows.at(name).exprs) {
//       if (expr.op == '?') {
        
//       } else {
//         if (expr.op == '<') {
//           // 0123456789012
//           // 0000111122222
//           // 1234
//           auto next_mask = mask;
//           for (size_t i{kVarPos.at(expr.var) + target + 1};
//                i < {kVarPos.at(expr.var) + kWide}; ++i) {
//             next_mask[i] = 0;
//           }
//           evaluate(p, expr.goto_workflow, masks, next_mask);
//         } else if (expr.op == '>') {
//           // 0123456789012
//           // 0000111122222
//           // 1234
//           auto next_mask = mask;
//           for (size_t i{kVarPos.at(expr.var)};
//                i <= {kVarPos.at(expr.var) + target}; ++i) {
//             next_mask[i] = 0;
//           }
//           evaluate(p, expr.goto_workflow, masks, next_mask);          
//         } else {
//           CHECK(false);
//         }
//       }
//     }
//   }
// }

uint64_t part2(const Problem &problem) {
  // std::vector<Mask> masks;
  // evaluate(problem, "in", res, Mask(4000 * 4, 1));
  // for (const auto& cond : res) {
  //   fmt::print("{}\n", cond);
  // }
  uint64_t answ{0};
  return answ;
}

int main(int argc, char **argv) {
  // fmt::print("{}\n", part1(parse("input19-test.txt")));
  // fmt::print("{}\n", part1(parse("input19.txt")));
  fmt::print("{}\n", part2(parse("input19-test.txt")));
  // fmt::print("{}\n", part2(parse("input19.txt")));
  return 0;
}