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

struct grid {
    char& get(int x, int y) { return points[y*width + x]; }

    const char& get(int x, int y) const { return points[y*width + x]; }

    bool corner(int x, int y) const {
        return (x==0 && y==0) || (x==width-1 && y==0) || (x==width-1 && y==height-1) || (x==0 && y==height-1);
    }

    bool on(int x, int y, bool complex) const { 
        if (complex && corner(x,y)) {
            return true;
        }
        return (x>=0 && y>=0 && x<width && y<height) ? get(x, y)=='#' : false; 
    }

    vector<char> points;
    int width = 0, height = 0;
};

const static int STEPS = 100;

const static vector<int> DIFF = {-1, 0, 1};

int solve(const grid &in, bool complex) {
    auto g = in;
    if (complex) {
        g.get(0          , 0           ) = '#';
        g.get(g.width - 1, 0           ) = '#';
        g.get(0          , g.height - 1) = '#';
        g.get(g.width - 1, g.height - 1) = '#';
    }

    auto d = g;

    for (int i = 0; i < STEPS; i++) {
        for (int y = 0; y < g.height; y++) {
            for (int x = 0; x < g.width; x++) {
                if (complex && g.corner(x, y)) {
                    continue;
                }

                int on_c = - g.on(x, y, complex);
                for (auto dy : DIFF) {
                    for (auto dx : DIFF) {
                        on_c += g.on(x + dx, y + dy, complex);
                    }
                }

                if (g.on(x, y, complex) && !(on_c == 2 || on_c == 3)) {
                    d.get(x, y) = '.';
                } else if (on_c == 3) {
                    d.get(x, y) = '#';
                }
            }
        }
        g = d;
    }

    return count(g.points.begin(), g.points.end(), '#');
}

int main() {
    ifstream input_data("inputs/q18.txt");
    string line;
    grid g;
    while(getline(input_data, line)) {
        g.points.insert(g.points.end(), line.begin(), line.end());
        g.height++;
        g.width = max(g.width, (int)line.size());
    }

    cout << solve(g, false) << endl;
    cout << solve(g, true) << endl;

    return 0;
}