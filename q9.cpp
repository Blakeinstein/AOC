#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

bool check_sum(vector<long int> &numbers, unordered_set<long int> &partial, int start, int end, long int value){
    for(auto i : partial)
        if ((value - i != value) && partial.find(value - i) != partial.end()) return true;
    return false;
}

int first_invalid(vector<long int> &numbers, int preamble_size) {
    int i;
    unordered_set<long int> partial;

    for(i=0; i<preamble_size; i++)
        partial.insert(numbers[i]);

    while(i < numbers.size()) {
        long int x = numbers[i];
        if (!check_sum(numbers, partial, i - preamble_size, i, x)) break;
        partial.erase(numbers[i-preamble_size]);
        partial.insert(x);
        i++;
    }
    return i;
}

tuple<int, int> find_sum_set(vector<long int> &numbers, long int target){
    int i=0, j=2;
    long int current = numbers[0] + numbers[1];
    while(j < numbers.size()) {
        if (current < target)
            current += numbers[j++];
        else if (current > target)
            current -= numbers[i++];
        else
            break;
    }
    return tuple<int, int>{i, j};
}

long int minmax_sum(vector<long int> &numbers, int start, int end){
    long int minimum=numbers[start], maximum=numbers[start];
    for(int i=start; i<end; i++){
        long int x = numbers[i];
        if ( minimum > x) minimum = x;
        if ( maximum < x) maximum = x;
    }
    return minimum+maximum;
}

int main() {
    ifstream input_data("inputs/q9.txt");
    string input;
    vector<long int> numbers;
    while(input_data >> input)
        numbers.push_back(stol(input));
    
    long int fault = numbers[first_invalid(numbers, 25)];
    // cout << fault << endl;
    auto range = find_sum_set(numbers, fault);
    cout << minmax_sum(numbers, get<0>(range), get<1>(range)) << endl;
    return 0;
}