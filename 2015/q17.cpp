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

using boxes = vector<int>;

const static int LITERS = 150;

int main() {
    ifstream input_data("inputs/q17.txt");
    string line;
    boxes bxs;
    while(getline(input_data, line)) {
        bxs.push_back(stoi(line));
    }
    

    int count = 0;
    int min_size = INT_MAX;
    int min_count = 0;

    int n = bxs.size();
    int pn = 1 << n;
    for (int i=0; i < pn; i++) {
        boxes bx;
        for (int j=0; j < n; ++j) {
            if (i & (1 << j)) {
                bx.push_back(bxs[j]);
            }
        }
        if (accumulate(bx.begin(), bx.end(), 0) == LITERS) {
            if (bx.size() < min_size) {
                min_size = bx.size();
                min_count = 0;
            }
            if (bx.size() == min_size) {
                min_count++;
            }
            count++;
        }
    }

    cout << count << endl;
    cout << min_count << endl;

    return 0;
}