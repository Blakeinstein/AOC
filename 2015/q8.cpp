#include <vector>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <functional>
#include <set>
#include <map>
#include <queue>
#include <regex>
using namespace std;

static const regex REDUCE { R"(\\(\\|\"|x[0-9a-f]{2}))" };
static const regex EXPAND { R"(\"|\\)" };
using SI = sregex_iterator;

auto fn1 = [] (auto &s) -> int {
  return accumulate (SI { s.begin(), s.end(), REDUCE }, { }, 2, [](int v, auto &m) -> int { return v + m.length() - 1; });
};
auto fn2 = [] (auto &s) -> int {
  return 2 + distance (SI { s.begin(), s.end(), EXPAND }, { });
};

int main() {
    ifstream input_data("inputs/q8.txt");
    string line;

    size_t part1 = 0, part2 = 0;

    while(getline(input_data, line)) {
        part1 += fn1(line);
        part2 += fn2(line);
    }

    cout << part1 << endl;
    cout << part2 << endl;
        
    return 0;
}