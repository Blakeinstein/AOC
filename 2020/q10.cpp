#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

int longest_chain(vector<int> &joltages){
    int jol1 = 0, jol3 = 0;

    for(int i=0; i<joltages.size()-1; i++){
        jol1 += (joltages[i+1] - joltages[i] == 1);
        jol3 += (joltages[i+1] - joltages[i] == 3);
    }

    return jol1 * jol3;
}

long int arrangement_count(vector<int> &joltages, unordered_map<int, long int> &memoize, int branch_index){
    if (memoize.find(branch_index) != memoize.end()) return memoize[branch_index];
    if (branch_index >= joltages.size() - 1) return 1;

    int i=branch_index+1, pivot=joltages[branch_index];
    long int returnval = 0;
    while(i < joltages.size() && joltages[i] - pivot <= 3){
        returnval += arrangement_count(joltages, memoize, i++);
    }
    memoize[branch_index] = returnval;
    return returnval;
}

int main() {
    ifstream input_data("inputs/q10.txt");
    string input;
    vector<int> joltages;
    unordered_map<int, long int> memoize;
    joltages.push_back(0);
    while(input_data >> input)
        joltages.push_back(stoi(input));

    sort(joltages.begin(), joltages.end());
    joltages.push_back(joltages.back() + 3);

    auto result1 = longest_chain(joltages);
    // cout << result1 << endl;
    auto result2 = arrangement_count(joltages, memoize, 0);
    cout << result2 << endl;
    return 0;
}