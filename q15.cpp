#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

long int elven_number(vector<int> &input, long int nth){
    unordered_map<long int, long int> last_spoken;
    long int i=0, last_number, second_last;
    for(; i<input.size() - 1; i++)
        last_spoken[input[i]] = i;
    
    last_number = input[i];
    second_last = input[i];

    while(i<nth-1){
        if (last_spoken.find(last_number) == last_spoken.end())
            last_number = 0;
        else
            last_number = i - last_spoken[last_number];
        last_spoken[second_last] = i++;
        second_last = last_number;
    }

    return last_number;
}

int main() {
    vector<int> input{0, 14, 6, 20, 1, 4};

    auto result1 = elven_number(input, 2020);
    cout << "part1) " << result1 << endl;
    auto result2 = elven_number(input, 30000000);
    cout << "part2) " << result2 << endl;

    return 0;
}