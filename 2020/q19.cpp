#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef vector<string> vs;
typedef unordered_set<string> uss;
typedef unordered_map<int, string> ums;

string build_rule(int key, ums &memory, ums &rules, int d11, int d8){
    if (memory.find(key) != memory.end()) return memory[key];
    string rule, temp;
    if (key == 8){
        d8++;
        if (d8 >= 7) return build_rule(42, memory, rules, d11, d8);
    } else if (key == 11){
        d11++;
        if (d11 >= 7) return build_rule(42, memory, rules, d11, d8) + build_rule(31, memory, rules, d11, d8);
    }
    int por=rules[key].find('|'),
        papos=rules[key].find("\"");
    if (por != string::npos){
        istringstream subrule1(rules[key].substr(0, por-1)),
                      subrule2(rules[key].substr(por+1, rules[key].length()));
        rule += "(";
        while(subrule1 >> temp){
            rule += build_rule(stoi(temp), memory, rules, d11, d8);
        }
        rule += "|";
        while(subrule2 >> temp){
            rule += build_rule(stoi(temp), memory, rules, d11, d8);
        }
        rule += ")";
    } else if (papos != string::npos){
        rule += rules[key][papos+1];
    } else {
        istringstream subrule(rules[key]);
        while(subrule >> temp){
            rule += build_rule(stoi(temp), memory, rules, d11, d8);
        }
    }
    memory[key] = rule;
    return rule;
}

int ques1(ums &rules, vs &messages){
    ums memory;
    string rule = build_rule(0, memory, rules, 0, 0);
    rule += "[\\s]*";
    regex re(rule);

    smatch match;
    int count = 0;
    for(auto x: messages){
        if (regex_match(x, match, re)){
            count++;
        }
    }
    return count;
}

int ques2(ums &rules, vs &messages){
    rules[8] = "42 | 42 8";
    rules[11] = "42 31 | 42 11 31";
    return ques1(rules, messages);
}

int main() {
    ifstream input_data("inputs/q19.txt");
    string input;
    vs messages;
    ums rules;
    while(getline(input_data, input)){
        if (input == "\r") break;
        int poc = input.find(":");
        int key = stoi(input.substr(0, poc));
        rules[key]=input.substr(poc+1, input.length()-1);
    }
    while(getline(input_data, input))
        messages.push_back(input);

    auto result1 = ques1(rules, messages);
    cout << "part1) " << result1 << endl;
    
    auto result2 = ques2(rules, messages);
    cout << "part2) " << result2 << endl;
        
    return 0;
}