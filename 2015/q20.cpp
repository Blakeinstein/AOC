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

int div_sum(int idx) {
    int sum = 0;
    for (int i = 1; i <= sqrt(idx); i++) {
        if (idx % i == 0) {
            sum += i;
            int d = idx / i;
            if (d != i) {
                sum += d;
            }
        }
    }

    return sum;
}

int div_sum2(int idx) {
    int sum = 0;
    for (int i = 1; i <= sqrt(idx); i++) {
        if (idx % i == 0) {
            int d = idx / i;
            if (d <= 50) {
                sum += i;
            }
            if (d!=i && i <= 50) {
                sum += d;
            }
        }
    }

    return sum;
}

int main() {
    int input = 36000000;

    int i = 1;
    while (true) {
        int present = 10 * div_sum(i);
        if (present >= input) {
            break;
        }
        i++;
    }

    cout << i << endl;

    i = 1;
    while (true) {
        int present = 11 * div_sum2(i);
        if (present >= input) {
            break;
        }
        i++;
    }

    cout << i << endl;

    return 0;
}