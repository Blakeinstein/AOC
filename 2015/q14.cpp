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

struct deer {
    string name;
    int velocity,
        run_time,
        rest_time,
        pos = 0,
        run_count = 0,
        rest_count = 0,
        score = 0;
};

using deers = vector<deer>;

static const int SECONDS = 2503;

int main() {
    ifstream input_data("inputs/q14.txt");
    string line;
    deers ds;
    while(getline(input_data, line)) {
        deer d;
        char str0[20];
        sscanf(
            line.c_str(),
            "%s can fly %d km/s for %d seconds, but then must rest for %d seconds.",
            str0, &d.velocity, &d.run_time, &d.rest_time
        );
        d.name = str0;
        d.run_count = d.run_time;
        d.rest_count = d.rest_time;
        ds.push_back(d);
    }


    auto ds1 = ds;
    auto max_dist = 0;
    for (auto &d : ds1) {
        float run_rest_ratio = d.run_time / (float)d.rest_time;
        float secs = SECONDS / (float)(d.rest_time + d.run_time);
        float progress = secs - floor(secs);
        bool resting = progress > run_rest_ratio;
        d.pos += d.velocity * d.run_time * (int)ceil(secs * (resting ? 1 : progress));
        max_dist = max(d.pos, max_dist);
    }

    cout << max_dist << endl;

    for (int i = 0; i < SECONDS; i++) {
        auto curr_max = 0;
        for (auto &d : ds) {
            if (d.run_count > 0) {
                d.pos += d.velocity;
                d.run_count--;
                if (d.run_count == 0) {
                    d.rest_count = d.rest_time;
                }
            } else {
                d.rest_count--;
                if (d.rest_count == 0) {
                    d.run_count = d.run_time;
                }
            }
            curr_max = max(d.pos, curr_max);
        }

        for (auto &d : ds) {
            if (d.pos >= curr_max) {
                d.score++;
            }
        }
    }

    auto max_score = 0;

    for (auto &d : ds) {
        max_score = max(d.score, max_score);
    }

    cout << max_score << endl;

    return 0;
}