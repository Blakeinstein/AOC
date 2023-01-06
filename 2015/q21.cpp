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

struct stats {
    int hp,
        dmg,
        arm;
};

struct item {
    string name;
    int cost,
        dmg,
        arm;
};


const static stats BOSS = { 103, 9, 2 };

void attack(const stats &p1, stats &p2) {
    p2.hp -= max(1, p1.dmg  - p2.arm);
}

bool fight(stats &player) {
    bool p_turn = true;
    auto enemy = BOSS;
    while (player.hp > 0 && enemy.hp > 0) {
        if (p_turn) {
            attack(player, enemy);
        } else {
            attack(enemy, player);
        }
        p_turn = !p_turn;
    }

    return player.hp > 0;
}

int main() {

    vector<item> weapons = {
        { "Dagger",  8, 4, 0 },
        { "Shortsword", 10, 5, 0 },
        { "Warhammer", 25, 6, 0 },
        { "Longsword", 40, 7, 0 },
        { "Greataxe", 74, 8, 0 }
    };

    vector<item> armors = {
        { "Naked", 0, 0, 0 },
        { "Leather", 13, 0, 1 },
        { "Chainmail", 31, 0, 2 },
        { "Splintmail", 53, 0, 3 },
        { "Bandedmail", 75, 0, 4 },
        { "Platemail", 102, 0, 5 }
    };

    vector<item> rings = {
        { "LeftRingless", 0, 0, 0 },
        { "RightRingless", 0, 0, 0 },
        { "Damage +1", 25, 1, 0 },
        { "Damage +2", 50, 2, 0 },
        { "Damage +3", 100, 3, 0 },
        { "Defense +1", 20, 0, 1 },
        { "Defense +2", 40, 0, 2 },
        { "Defense +3", 80, 0, 3 }
    };

    int min_gold = INT_MAX;
    int max_gold = 0;

    for (const auto &wep : weapons) {
        for (const auto &arm : armors) {
            for (const auto &r1 : rings) {
                for (const auto &r2 : rings) {
                    if (r1.name == r2.name) {
                        continue;
                    }

                    stats player {
                        100,
                        wep.dmg + r1.dmg + r2.dmg,
                        arm.arm + r1.arm + r2.arm,
                    };

                    int cost = wep.cost + arm.cost + r1.cost + r2.cost;

                    if (fight(player)) {
                        min_gold = min(min_gold, cost);
                    } else {
                        max_gold = max(max_gold, cost);
                    }
                }
            }
        }
    }

    cout << min_gold << endl;
    cout << max_gold << endl;

    return 0;
}