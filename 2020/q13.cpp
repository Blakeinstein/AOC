#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

regex idp("([0-9x]+)");

int earliest(int timeslot, vector<string> &ids){
    int id = timeslot, time = timeslot;
    for(auto mnid : ids){
        if (mnid == "x") continue;
        int cid = stoi(mnid);
        int ctime = cid - timeslot % cid;
        if (ctime < time){
            time = ctime;
            id = cid;
        }
    }
    return id * time; 
}

int inverse(int modulus, int factor){
    int n = 0, step = factor % modulus;
    while((step*n)%modulus != 1) n++;
    return n;
}

long long int common(vector<string> &ids){
    long long int t=0, step = 1;
    
    for(int i=0; i<ids.size(); i++){
        if (ids[i] == "x") continue;
        int cid = stoi(ids[i]);
        while( (t+i) % cid != 0)
            t += step;
        step *= cid;
    }

    return t;
}

int main() {
    ifstream input_data("inputs/q13.txt");
    string input;
    input_data >> input;
    int timeslot = stoi(input);
    vector<string> ids;
    input_data >> input;
    
    smatch match;
    while(regex_search(input, match, idp)){
        ids.push_back(match.str(1));
        input=match.suffix().str();
    }

    // auto result1 = earliest(timeslot, ids);
    // cout << result1 << endl;

    auto result2 = common(ids);
    printf("%llu", result2);
    return 0;
}