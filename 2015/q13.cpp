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

struct edge {
    string from, to;
    int delta;
};

using edges = vector<edge>;
using graph = map<string, map<string, edge>>;

int mod(int a, int b) {
    return (b + (a%b)) % b;
}

auto calc_max_delta(const graph &g, const set<string> &guests) {
    int max_delta = 0;
    vector<string> names(guests.begin(), guests.end());
    do {
        int local = 0;
        for (int i = 0; i < names.size(); i++) {
            local += g.at(names[i]).at(names[mod(i+1, (int)names.size())]).delta;
            local += g.at(names[i]).at(names[mod(i-1, (int)names.size())]).delta;
        }
        max_delta = max(local, max_delta);
    } while (next_permutation(names.begin(), names.end()));
    return max_delta;
}

int main() {
    ifstream input_data("inputs/q13.txt");
    edges eds;
    string line;
    while(getline(input_data, line)) {
        edge ed;
        char str0[20];
        char str1[20];
        char str2[20];
        sscanf(line.c_str(), "%s would %s %d happiness units by sitting next to %s.", str0, str1, &ed.delta, str2);
        ed.from = str0;
        ed.to = str2;
        if (strcmp(str1, "lose") == 0) {
            ed.delta *= -1;
        }
        if (ed.to.back() == '.') {
            ed.to.pop_back();
        }
        eds.push_back(ed);
    }

    set<string> guests;
    graph g;
    for (auto &ed : eds) {
        g[ed.from][ed.to] = {ed.from, ed.to, ed.delta};
        guests.insert(ed.from);
    }

    cout << calc_max_delta(g, guests) << endl;

    for(auto& name : guests){
        g[name]["me"] = {name, "me", 0};
        g["me"][name] = {"me", name, 0};
    }
    guests.insert("me");

    cout << calc_max_delta(g, guests) << endl;

    return 0;
}