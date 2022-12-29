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

string look_n_say(const string last) {
    stringstream ans;
    int c = 1;
    char last_c = last[0];
    for (int i = 1; i < last.size(); i++, c++) {
        if (last[i] != last_c) {
            ans << c << last_c;
            c=0;
            last_c = last[i];
        }
    }

    ans << c << last_c;

    return ans.str();
}

int main() {
    string inp = "1321131112";

    for (int i = 0; i < 40; i++) {
        inp = look_n_say(inp);
    }

    cout << inp.length() << endl;

    for (int i = 0; i < 10; i++) {
        inp = look_n_say(inp);
    }

    cout << inp.length() << endl;

    return 0;
}