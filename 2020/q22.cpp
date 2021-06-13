#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef long long int llt;

template<typename T>
T yield(queue<T> &s){
    T out = s.front();
    s.pop();
    return out;
}
template<typename T>
T yield(deque<T> &s){
    T out = s.front();
    s.pop_front();
    return out;
}

llt ques1(vector<int> &player1v, vector<int> &player2v){
    queue<int> player1, player2, *winner;
    for(auto x: player1v)
        player1.push(x);
    for(auto x: player2v)
        player2.push(x);

    int depth=0;
    while(!player1.empty() && !player2.empty() && depth < 20000){
        int p1 = yield(player1), p2 = yield(player2);
        if (p1 > p2){
            player1.push(p1);
            player1.push(p2);
        } else {
            player2.push(p2);
            player2.push(p1);
        }
        depth++;
    }

    if(player1.empty()) winner = &player2;
    else winner = &player1;

    int multiplier = winner->size();
    llt ans = 0;
    while(!winner->empty())
        ans += yield(*winner) * (multiplier--);
    return ans;
}

// unordered_map<int, vector<vector<queue<int> > > > memory;
llt calc_hash(vector<deque<int> > &decks){
    size_t seed = decks[0].size() + decks[1].size();
    for(auto& i : decks[0]) {
        seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    for(auto& i : decks[1]) {
        seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
}

bool recc(vector<deque<int> > &decks) {
    unordered_set<llt> memory;
    while(!decks[0].empty() && !decks[1].empty()){
        llt chash = calc_hash(decks);
        if (memory.find(chash) != memory.end())
            return false;
        memory.insert(chash);
        int p1 = yield(decks[0]), p2 = yield(decks[1]);
        vector<deque<int> > deckcopy(decks);
        bool win;
        if (p1<=decks[0].size() && p2<=decks[1].size()){
            win = recc(deckcopy);
        } else {
            win = p2 > p1;
        }
        if (win){
            decks[1].push_back(p2);
            decks[1].push_back(p1);
        } else {
            decks[0].push_back(p1);
            decks[0].push_back(p2);
        }
    }
    return decks[0].empty();
}

llt ques2(vector<int> &player1v, vector<int> &player2v){
    vector<deque<int> > decks;
    
    deque<int> temp, temp2;
    for(auto x: player1v)
        temp.push_back(x);
    decks.push_back(temp);
    for(auto x: player2v)
        temp2.push_back(x);
    decks.push_back(temp2);

    bool winner = recc(decks);
    int multiplier = decks[winner].size();
    llt ans = 0;
    
    while(!decks[winner].empty()){
        ans += yield(decks[winner]) * (multiplier--);
    }
    return ans;
}

int main() {
    ifstream input_data("inputs/q22.txt");
    string input;
    vector<int> player1v, player2v;
    getline(input_data, input);
    while(getline(input_data, input)){
        if (input == "\r") break;
        player1v.push_back(stoi(input));
    }
    getline(input_data, input);
    while(getline(input_data, input)){
        if (input == "\r") break;
        player2v.push_back(stoi(input));
    }

    auto result1 = ques1(player1v, player2v);
    cout << "part1) " << result1 << endl;
    
    auto result2 = ques2(player1v, player2v);
    cout << "part2) " << result2 << endl;
    return 0;
}