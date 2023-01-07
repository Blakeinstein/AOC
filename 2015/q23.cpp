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

struct instruction {
    string opr, op1, op2;
};

using instructions = vector<instruction>;

struct program {
    program(instructions ins) : ins(ins) {}

    void run() {
        while (pc < ins.size()) {
            exec(ins[pc]);
        }
    }

    void exec(instruction &in) {
        if (in.opr == "hlf") {
            regs[in.op1] /= 2;
        } else if (in.opr == "tpl") {
            regs[in.op1] *= 3;
        } else if (in.opr == "inc") {
            regs[in.op1]++;
        } else if (in.opr == "jmp") {
            pc += get(in.op1) - 1;
        } else if (in.opr == "jie") {
            if (get(in.op1) % 2 == 0) {
                pc += get(in.op2) - 1;
            }
        } else if (in.opr == "jio") {
            if (get(in.op1) == 1) {
                pc += get(in.op2) - 1;
            }
        }

        pc++;
    }

    int get(string &s) {
        return (s[0] >= 'a' && s[0] <= 'z') ? regs[s] : stoll(s);
    }

    int value() const {
        return regs.at("b");
    }

    instructions ins;
    map<string, int> regs;
    size_t pc;
};

int main() {
    ifstream input_data("inputs/q23.txt");
    string line;
    instructions ins;
    while(getline(input_data, line)) {
        instruction in;
        stringstream ss(line);
        ss >> in.opr >> in.op1 >> in.op2;
        if (in.op1.back() == ',') {
            in.op1.pop_back();
        }
        ins.push_back(in);  
    }

    program prog(ins);
    prog.run();
    cout << prog.value() << endl;

    prog = program(ins);
    prog.regs["a"] = 1;
    prog.run();

    cout << prog.value() << endl;

    return 0;
}