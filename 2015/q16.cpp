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

struct sue {
    int num;
    map<string, int> things;
};

using sues = vector<sue>;

int main() {
    ifstream input_data("inputs/q16.txt");
    string line;
    sues ss;

    set<int> poss1;
    set<int> poss2;
    while(getline(input_data, line)) {
        sue s;
        char str0[20];
        char str1[20];
        char str2[20];
        int a, b, c;
        sscanf(
            line.c_str(),
            "Sue %d: %s %d, %s %d, %s %d",
            &s.num, str0, &a, str1, &b, str2, &c
        );
        s.things[str0] = a;
        s.things[str1] = b;
        s.things[str2] = c;
        poss1.insert(s.num);
        poss2.insert(s.num);
        ss.push_back(s);
    }

    map<string, int> our_sue = {
        {"children:", 3},
        {"cats:", 7},
        {"samoyeds:", 2},
        {"pomeranians:", 3},
        {"akitas:", 0},
        {"vizslas:", 0},
        {"goldfish:", 5},
        {"trees:", 3},
        {"cars:", 2},
        {"perfumes:", 1},
    };

    for (auto s : ss) {
        for (auto &[thing, num] : s.things) {
            if (thing == "cats:" || thing == "trees:") {
                if (our_sue[thing] >= num) {
                    poss2.erase(s.num);
                }
            } else if (thing == "pomeranians:" || thing == "goldfish:") {
                if (our_sue[thing] <= num) {
                    poss2.erase(s.num);
                }
            } else if (our_sue[thing] != num) {
                poss2.erase(s.num);
            }
            if (our_sue[thing] != num) {
                poss1.erase(s.num);
            }
        }
    }

    cout << *poss1.begin() << endl;
    cout << *poss2.begin() << endl;

    return 0;
}