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

int main() {
    ifstream input_data("inputs/template.txt");
    string line;
    while(getline(input_data, line)) {
        cout << line << endl;
    }

    return 0;
}