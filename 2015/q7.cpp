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

struct instruction{
    string in0;
    string in1;
    string out;
    string op;
};

bool is_wire(const string &s) {
    return s[0] >= 'a' && s[0] <= 'z';
}

unsigned short fetch(map<string, instruction> &ins, map<string, unsigned short> &wires, const string &wire) {
    if (!is_wire(wire)) {
        return stoi(wire);
    }

    if (wires.count(wire)) {
        return wires[wire];
    }

    auto &in = ins.at(wire);

    if (in.op == "SET") {
        wires[in.out] = fetch(ins,wires,in.in0);
    } else if (in.op == "NOT") {
        wires[in.out] = ~fetch(ins,wires,in.in0);
    } else if (in.op == "AND") {
        wires[in.out] = fetch(ins,wires,in.in0) & fetch(ins,wires,in.in1);
    } else if (in.op == "OR") {
        wires[in.out] = fetch(ins,wires,in.in0) | fetch(ins,wires,in.in1);
    } else if (in.op == "LSHIFT") {
        wires[in.out] = fetch(ins,wires,in.in0) << fetch(ins,wires,in.in1);
    } else if (in.op == "RSHIFT") {
        wires[in.out] = fetch(ins,wires,in.in0) >> fetch(ins,wires,in.in1);
    }

    return wires[in.out];
}

int main() {
    ifstream input_data("inputs/q7.txt");
    string line;
    map<string, instruction> ins;
    while(getline(input_data, line)) {
        instruction in;
        char str0[20];
        char str1[20];
        char str2[20];
        char str3[20];
        auto nspaces = std::count(line.begin(),line.end(),' '); 
        if (nspaces == 2) {
            sscanf(line.c_str(), "%s -> %s", str0, str1);
            in.op = "SET";
            in.in0 = str0;
            in.out = str1;
        } else if (nspaces == 3) {
            sscanf(line.c_str(), "%s %s -> %s", str0, str1, str2);
            in.op = str0;
            in.in0 = str1;
            in.out = str2;
        } else if (nspaces == 4) {
            sscanf(line.c_str(), "%s %s %s -> %s", str0, str1, str2, str3);
            in.in0 = str0;
            in.op = str1;
            in.in1 = str2;
            in.out = str3;
        }
        ins[in.out] = in;
    }

    map<string, unsigned short> wires;
    auto part1 = fetch(ins, wires, "a");

    wires = {{"b", part1}};
    auto part2 = fetch(ins, wires, "a");

    cout << part1 << endl;
    cout << part2 << endl;
        
    return 0;
}