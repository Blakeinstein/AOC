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

void increment(string &old) {
    auto i = old.size() - 1;

    while ( i > 0 && old[i] == 'z') {
        old[i] = 'a';
        i--;
    }

    old[i]++;
}

bool isValid(const string &pass) {
    if (pass.contains('i') || pass.contains('o') || pass.contains('l')) {
        return false;
    }

    size_t i = 2;
    char repeat = pass[i-2] == pass[i-1] ? pass[i-1] : ' ';
    size_t has_pair = repeat != ' ';
    bool has_inc = false;

    while (i < pass.size()) {
        if (repeat != pass[i-1] && pass[i-1] == pass[i]) {
            has_pair++;
            repeat = pass[i-1];
        }
        has_inc |= pass[i-2] + 1 == pass[i-1] && pass[i-1] + 1 == pass[i];
        i++;
    }

    return has_pair > 1 && has_inc;
}

int main() {
    string input = "hepxcrrq";

    do {
        increment(input);
    } while (!isValid(input));

    cout << input << endl;

    do {
        increment(input);
    } while (!isValid(input));

    cout << input << endl;

    return 0;
}