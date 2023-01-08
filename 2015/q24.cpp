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

size_t sum(const vector<int> &vec) {
    return accumulate(vec.begin(), vec.end(), 0);
}

size_t prod(const vector<int> &vec) {
    return accumulate(vec.begin(), vec.end(), 1ull, multiplies<size_t>());
}

int solve(const vector<int> &packages, int groups) {
    auto sum_t = sum(packages);
    auto sum_n = sum_t / groups;

    int set_size = 1;
    size_t ans = ULLONG_MAX;

    while (ans == ULLONG_MAX) {
        vector<uint8_t> bitset(set_size, 1);
        bitset.resize(packages.size(), 0);
        vector<int> sub_set(set_size);

        do {
            int j = 0;
            for (size_t i = 0; i < packages.size(); i++) {
                if (bitset[i]) {
                    sub_set[j++] = packages[i];
                }
            }
            if (sum(sub_set) == sum_n) {
                ans = min(ans, prod(sub_set));
            }
        } while (prev_permutation(bitset.begin(), bitset.end()));
        
        set_size++;
    }

    return ans;
}

int main() {
    ifstream input_data("inputs/q24.txt");
    string line;
    vector<int> nums;
    while(getline(input_data, line)) {
        nums.push_back(stoi(line));
    }

    cout << solve(nums, 3) << endl;
    cout << solve(nums, 4) << endl;

    return 0;
}