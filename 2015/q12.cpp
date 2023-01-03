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
#include "../json.hpp"
using namespace std;
using namespace nlohmann;

static const regex NUMBER { "-?[0-9]+" };
using SI = sregex_iterator;

int part1(auto &s) {
    return accumulate (SI { s.begin(), s.end(), NUMBER }, { }, 0, [](int v, auto &m) -> int { return v + stoi(m.str()); });
}

int part2(const json &j) {
    if (j.is_number()) {
        return int(j);
    }

    int res = 0;

    if (j.is_object() || j.is_array()) {
        for (auto k : j.items()) {
            auto val = k.value();
            if (j.is_object() && val.is_string() && val == "red") {
                return 0;
            }
            res += part2(val);
        }
    }

    return res;
} 

int main() {
    ifstream input_data("inputs/q12.txt");
    string input;
    assert(getline(input_data, input));

    cout << part1(input) << endl;

    input_data.clear();
    input_data.seekg(0, std::ios::beg);

    cout << part2(json::parse(input_data)) << endl;
    return 0;
}