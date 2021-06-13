#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

int pairSum(vector<int> array, int targetSum) {
    unordered_set<int> target;

    for (int i : array)
        if (targetSum - i > 0)
            target.insert(targetSum - i);

    for (int x : array)
        if (target.find(x) != target.end()) {
            return x;
        }

    return -1;
}


tuple<int, int> triSum(vector<int> array, int targetSum) {
    vector<int> target;
    for (int i : array)
        if (targetSum - i > 0)
            target.push_back(targetSum - i);

    int y;
    for (int x : target) 
        if ((y = pairSum(array, x)) > -1)
            return tuple<int, int>{x - y, y};
    
    return tuple<int, int>{0, 0};
}

int main(){
    vector<int> a;
    ifstream input_data("inputs/q1.txt");  
    int input;
    while(input_data >> input)
        a.push_back(input);
    int target = 2020;

    int ans = pairSum(a, target);
    cout << ans * (target - ans) << "\n";
    auto ans2 = triSum(a, target);
    cout << get<0>(ans2) * get<1>(ans2) * (target - get<0>(ans2) - get<1>(ans2)) << "\n";
    
    return 0;
}