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

int main() {
    ifstream input_data("inputs/template.txt");
    string input;
    while(getline(input_data, input))
        cout << input << endl;
        
    return 0;
}