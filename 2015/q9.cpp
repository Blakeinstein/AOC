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
    int dist;
};

using edges = vector<edge>;
using graph = map<string, edges>;

void min_max_dst(const graph &g, const string &v, int curr_dst, int &min_dst, int &max_dst, set<string> &path, const set<string> &dst) {
    if (path == dst) {
        min_dst = min(min_dst, curr_dst);
        max_dst = max(max_dst, curr_dst);
        return;
    }
    for (auto &ed : g.at(v)) {
        if (!path.count(ed.to)) {
            path.insert(ed.to);
            min_max_dst(g, ed.to, curr_dst + ed.dist, min_dst, max_dst, path, dst);
            path.erase(ed.to);
        }
    }
}

int main() {
    ifstream input_data("inputs/q9.txt");
    edges eds;
    string line;
    set<string> dst;
    graph g;

    while(getline(input_data, line)) {
        edge ed;
        char str0[20];
        char str1[20];
        sscanf(line.c_str(), "%s to %s = %d", str0, str1, &ed.dist);
        ed.from = str0;
        ed.to = str1;
        eds.push_back(ed);
        g[ed.from].push_back({ed.from, ed.to, ed.dist});
        g[ed.to].push_back({ed.to, ed.from, ed.dist});
        dst.insert(ed.from);
        dst.insert(ed.to);
    }

    int min_dst = INT_MAX;
    int max_dst = 0;
    for (auto &loc : dst) {
        set<string> path{loc};
        min_max_dst(g, loc, 0, min_dst, max_dst, path, dst);
    }

    cout << min_dst << endl;
    cout << max_dst << endl;

    return 0;
}