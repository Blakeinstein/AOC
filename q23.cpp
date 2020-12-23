#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef unsigned long long int llt;

struct node{
    int value;
    struct node *next;
};

void simulation(vector<int> &cups, unordered_map<int, node> &map, int steps){
    node *current = &map[cups[0]];
    for(int i=0; i<steps; i++){
        node *picked = current->next;
        current->next = current->next->next->next->next;
        int label = current->value;
        while(label == current->value || label == picked->value || label == picked->next->value)
            label = label!=1? label - 1 : cups.size();
        node *dest = &map[label];
        picked->next->next->next = dest->next;
        current = current->next;

    }
}

string ques1(string label){
    vector<int> cups;
    unordered_map<int, node> map;
    for(auto c: label){
        cups.push_back(c - '0');
    }

    for(int i=0; i<label.length(); i++){
        node temp;
        temp.value = i;
        map[i] = temp;
    }

    for(int i=0; i<label.length(); i++)
        map[i].next = &(map[(i+1)%label.length()]);

    simulation(cups, map, 100);
    node *p;
    string ans;
    for(int i=0; i<label.length()-1; i++){
        p = p->next;
        ans += ('0' + p->value);
    }
    return ans;
}

llt ques2(string label){
    vector<int> cups;
    unordered_map<int, node> map;
    for(auto c: label){
        cups.push_back(c - '0');
    }

    for(int i=label.length(); i<1000001; i++){
        cups.push_back(i);
    }

    for(int i=0; i<1000001; i++){
        node temp;
        temp.value = i;
        map[i] = temp;
    }

    for(int i=0; i<1000001; i++)
        map[i].next = &(map[(i+1)%label.length()]);

    simulation(cups, map, 1000000);

    return map[1].next->value * map[1].next->next->value;
}

int main() {
    auto result1 = ques1("389125467");
    cout << "part1) " << result1 << endl;
    
    auto result2 = ques2("389125467");
    cout << "part2) " << result2 << endl;
        
    return 0;
}