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

struct point {
    int col, row;
};

int main() {
    int row = 2947, col = 3029;
    size_t next = 20151125;

    int ymax = 2;
    point ipos{1, 2}, pos{col, row};

    while (true) {
        next = (next * 252533) % 33554393;

        if (ipos.row == pos.row && ipos.col == pos.col) {
            break;
        }

        ipos.col++;
        ipos.row--;

        if (ipos.row == 0) {
            ymax++;
            ipos.row = ymax;
            ipos.col = 1;
        }
    }

    cout << next << endl;

    return 0;
}