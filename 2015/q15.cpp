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

struct properties {
    int capacity = 0,
        durability = 0,
        flavor = 0,
        texture = 0,
        calories = 0;
};

struct ingredient {
    properties prop;
    string name;
};

using ingredients = vector<ingredient>;

int max1 = 0;
int max2 = 0;

void recursive(const ingredients &ings, vector<int> &tsps, int ts) {
    if (ts < tsps.size()) {
        for (int i=tsps[ts-1]; i<=100; i++) {
            tsps[ts] = i - tsps[ts-1];
            if (tsps[ts] != 0) {
                recursive(ings, tsps, ts+1);
            }
        }
        return;
    }

    if (accumulate(tsps.begin(), tsps.end(), 0) != 100) {
        return;
    }

    properties prop;
    for (int j=0; j < ings.size(); j++) {
        prop.capacity += tsps[j] * ings[j].prop.capacity;
        prop.durability += tsps[j] * ings[j].prop.durability;
        prop.flavor += tsps[j] * ings[j].prop.flavor;
        prop.texture += tsps[j] * ings[j].prop.texture;
        prop.calories += tsps[j] * ings[j].prop.calories;
    }

    int score = max(0, prop.capacity) * max(0, prop.durability) * max(0, prop.flavor) * max(0, prop.texture);

    max1 = max(max1, score);
    if (prop.calories == 500) {
        max2 = max(max2, score);
    }
}

int main() {
    ifstream input_data("inputs/q15.txt");
    string line;
    ingredients ings;
    while(getline(input_data, line)) {
        ingredient ing;
        char str[20];
        sscanf(
            line.c_str(),
            "%s capacity %d, durability %d, flavor %d, texture %d, calories %d",
            str, &ing.prop.capacity, &ing.prop.durability, &ing.prop.flavor, &ing.prop.texture, &ing.prop.calories
        );
        ing.name = str;
        ing.name.pop_back();
        ings.push_back(ing);
    }

    vector<int> tsps(ings.size(), 0);
    for (int i=0; i < 100; i++) {
        tsps[0] = i;
        recursive(ings, tsps, 1);
    }

    cout << max1 << endl;
    cout << max2 << endl;

    return 0;
}