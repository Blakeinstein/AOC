#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

regex ins("mem\\[([0-9]+)\\] = ([0-9]+)");

void create_mask_set(unordered_map<int, int> &maskset, string mask){
    maskset.clear();
    for(int i = 0; i<mask.size() - 1; i++){
        if (mask[i] == 'X') continue;
        maskset[mask.size() - i - 2] = (mask[i] - '0')? 1 : 0;
    }
}

void update_state(unordered_map<int, int> &maskset, int addr, long int value, unordered_map<long long int, long int> &memory){
    bitset<36> bvalue(value);
    for(auto maskbit : maskset)
        bvalue[maskbit.first] = maskbit.second;
    memory[addr] = bvalue.to_ulong();
}

void memory_access_decoder(unordered_map<int, int> &maskset, int addr, long int value, unordered_map<long long int, long int> &memory){
    vector<long long int> addresses;
    vector<int>  hot_pos;
    bitset<36> baddr(addr);
    for(int i=0; i<baddr.size(); i++){
        if (maskset[i] == 2){
            baddr[i] = 0;
            hot_pos.push_back(i);   
        } else {
            baddr[i] = baddr[i] | maskset[i];
        }
    }
    long int len = 1 << hot_pos.size();
    long long int base_addr = baddr.to_ulong();
    for(long int i=0; i < len; i++){
        long int sum = 0;
        for(long int j=0; j<hot_pos.size(); j++)
            if(i & (1 << j)) sum += 1 << hot_pos[j];
        addresses.push_back(base_addr + sum);
    }

    for(auto i : addresses) memory[i] = value;
}

long long int final_state(unordered_map<long long int, long int> &memory){
    long long int sum = 0;
    for(auto x : memory) sum += x.second;
    return sum;
}

void create_floating_mask_set(unordered_map<int, int> &maskset, string mask){
    maskset.clear();
    for(int i = 0; i<mask.size() - 1; i++){
        
        if (mask[i] == 'X') maskset[mask.size() - i - 2] = 2 ;
        else maskset[mask.size() - i - 2] = (mask[i] - '0')? 1 : 0;
    }
}

int main() {
    ifstream input_data("inputs/q14.txt");
    string input, mask;
    smatch match;
    unordered_map<long long int, long int> memory;
    unordered_map<int, int> maskset;

    while(getline(input_data, input)){
        if(input.rfind("mask = ", 0) == 0)
            create_mask_set(maskset, input.substr(7, 7+36));
        else{
            regex_search(input, match, ins);
            update_state(maskset, stoi(match.str(1)), stol(match.str(2)), memory);
        }
    }

    auto result1 = final_state(memory);
    cout << "part1) " << result1 << endl;
    ifstream input_data("inputs/q14.txt");
    memory.clear();
    while(getline(input_data, input)){
        if(input.rfind("mask = ", 0) == 0)
            create_floating_mask_set(maskset, input.substr(7, 7+36));
        else{
            regex_search(input, match, ins);
            memory_access_decoder(maskset, stoi(match.str(1)), stol(match.str(2)), memory);
        }
    }
    auto result2 = final_state(memory);
    cout << "part2) " << result2 << endl;

    return 0;
}