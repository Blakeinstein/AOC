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
        damage;
};

struct board_state {
    stats boss;
    int mana = 500,
        player_hp = 50,
        shield = 0,
        shield_time = 0,
        poison_time = 0,
        recharge_time = 0,
        mana_cost = 0;
};

enum Spell { MAGIC_MISSILE = 0, DRAIN, SHIELD, POISON, RECHARGE };

bool update(board_state &st, int &min_cost) {
    st.shield_time--;
    st.poison_time--;
    st.recharge_time--;

    st.shield = st.shield_time >= 0 ? 7 : 0;
    
    if (st.poison_time >= 0) {
        st.boss.hp -= 3;
    }

    if (st.recharge_time >= 0) {
        st.mana += 101;
    }

    if (st.boss.hp <= 0) {
        min_cost = min(min_cost, st.mana_cost);
        return true;
    }
    return false;
}

bool cast_spell(board_state &st, Spell &sp, int &curr_min) {
    switch (sp) {
        case MAGIC_MISSILE: 
            st.boss.hp -= 4;
            st.mana -= 53;
            st.mana_cost += 53;
            break;
        case DRAIN:
            st.boss.hp -= 2;
            st.player_hp += 2;
            st.mana -= 73;
            st.mana_cost += 73;
            break;
        case SHIELD:
            if (st.shield_time >= 0) {
                return false;
            }
            st.mana -= 113;
            st.mana_cost += 113;
            st.shield_time = 6;
            break;
        case POISON:
            if (st.poison_time >= 0) {
                return false;
            }
            st.mana -= 173;
            st.mana_cost += 173;
            st.poison_time = 6;
            break;
        case RECHARGE:
            if (st.recharge_time >= 0) {
                return false;
            }
            st.mana -= 229;
            st.mana_cost += 229;
            st.recharge_time = 5;
    }
    return st.mana > 0 && curr_min > st.mana_cost;
}

void dfs(board_state st, Spell sp, int &min_cost, bool complex) {
    if (complex) {
        st.player_hp--;
    }

    if (st.player_hp <= 0) {
        return;
    }

    if (update(st, min_cost) || !cast_spell(st, sp, min_cost) || update(st, min_cost)) {
        return;
    }

    st.player_hp -= max(1, st.boss.damage - st.shield);

    dfs(st, MAGIC_MISSILE, min_cost, complex);
    dfs(st, DRAIN, min_cost, complex);
    dfs(st, SHIELD, min_cost, complex);
    dfs(st, POISON, min_cost, complex);
    dfs(st, RECHARGE, min_cost, complex);
}

auto solve(bool complex) {
    int min_cost = INT_MAX;

    board_state st { { 51, 9 } };

    dfs(st, MAGIC_MISSILE, min_cost, complex);
    dfs(st, DRAIN, min_cost, complex);
    dfs(st, SHIELD, min_cost, complex);
    dfs(st, POISON, min_cost, complex);
    dfs(st, RECHARGE, min_cost, complex);

    return min_cost;
}

int main() {

    cout << solve(false) << endl;
    cout << solve(true) << endl;

    return 0;
}