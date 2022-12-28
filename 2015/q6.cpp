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
using namespace std;

struct pos_t{
    int x,y;
};

struct instruction_t{
    string op;
    pos_t start, end;
};

struct grid_t{
    grid_t(int w,int h) : points(w*h, 0), width(w), height(h)  {}

    int& get(int x,int y) { return points[y*width+x]; }
    const int& get(int x,int y) const { return points[y*width+x]; }

    void toggle(const pos_t& s,const pos_t& e, bool part1){
        for(int y=s.y; y<=e.y; ++y){
            for(int x=s.x; x<=e.x; ++x){
                if (part1) {
                    get(x,y) = !get(x,y);
                } else {
                    get(x,y) += 2;
                }
            }
        }
    }

    void turn(const pos_t& s,const pos_t& e, bool onoff, bool part1){
        for(int y=s.y; y<=e.y; ++y){
            for(int x=s.x; x<=e.x; ++x){
                if (part1) {
                    get(x,y) = onoff;
                } else {
                    get(x,y) = std::max(get(x,y) + (onoff ? 1 : -1), 0);
                }
            }
        }
    }

    vector<int> points;
    int width, height;
};

int main() {
    ifstream input_data("inputs/q6.txt");
    string line;
    vector<instruction_t> ins;

    while(getline(input_data, line)) {
        instruction_t in;
        if (line.find("turn on") != string::npos) {
            in.op = "turn on";
            sscanf(line.c_str(), "turn on %d,%d through %d,%d", &in.start.x, &in.start.y, &in.end.x, &in.end.y);
        } else if (line.find("turn off") != string::npos) {
            in.op = "turn off";
            sscanf(line.c_str(), "turn off %d,%d through %d,%d", &in.start.x, &in.start.y, &in.end.x, &in.end.y);
        } else if (line.find("toggle") != string::npos) {
            in.op = "toggle";
            sscanf(line.c_str(), "toggle %d,%d through %d,%d", &in.start.x, &in.start.y, &in.end.x, &in.end.y);
        }
        ins.push_back(in);
    }

    grid_t gridA(1000, 1000);
    grid_t gridB(1000, 1000);

    for (auto &in : ins) {
        if (in.op == "turn on") {
            gridA.turn(in.start, in.end, true, true);
            gridB.turn(in.start, in.end, true, false);
        } else if (in.op == "turn off") {
            gridA.turn(in.start, in.end, false, true);
            gridB.turn(in.start, in.end, false, false);
        } else if (in.op == "toggle") {
            gridA.toggle(in.start, in.end, true);
            gridB.toggle(in.start, in.end, false);
        }
    }

    auto part1 = accumulate(gridA.points.begin(), gridA.points.end(), 0);
    auto part2 = accumulate(gridB.points.begin(), gridB.points.end(), 0);
    
    cout << part1 << endl;
    cout << part2 << endl;

    return 0;
}