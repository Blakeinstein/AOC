#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

regex rule("([a-z]+):([0-9a-zA-Z#]+)\\b");
regex hexcolor("^#([a-f0-9]{6})$");
unordered_set<string> ruleset{
    "byr", 
    "iyr", 
    "eyr", 
    "hgt", 
    "hcl", 
    "ecl", 
    "pid", 
    "cid", 
}, eyecolors{
    "amb",
    "blu",
    "brn",
    "gry",
    "grn",
    "hzl",
    "oth",
};

bool check_hex(string &color) {
    smatch match;
    return regex_match(color, match, hexcolor);
}

bool is_digits(string &str){
    return all_of(str.begin(), str.end(), ::isdigit);
}

int safetoi(string &year) {
    try {return stoi(year);}
    catch (invalid_argument e) {return 0;}
}

bool verify (string passport) {
    smatch match;
    sregex_iterator iter(passport.begin(), passport.end(), rule), end;
    unordered_set <string> rules;
    while (iter != end) {
        string rule = (*iter)[0];
        string value = (*iter)[1];
        rules.insert(rule.substr(0, 3));
        ++iter;
    }
    for (string rule : ruleset) {
        if(rules.find(rule) == rules.end() && rule != "cid")
            return false;
    }
    return true;
}
bool strict_verify (string passport) {
    smatch match;
    sregex_iterator iter(passport.begin(), passport.end(), rule), end;
    unordered_map <string, string> rules;
    while (iter != end) {
        string rule = (*iter)[0];
        string value = rule.substr(4, rule.length()-4);
        rules[rule.substr(0, 3)] = value;
        ++iter;
    }
    for (string rule : ruleset) {
        if(rules.find(rule) == rules.end() && rule != "cid")
            return false;
        else {
            if (rule == "byr"){
                int year = safetoi(rules[rule]);
                if (rules[rule].length() == 4 && (year >= 1920 && year <= 2002))
                    continue;
                else return false;
            }
            else if (rule == "iyr"){
                int year = safetoi(rules[rule]);
                if (rules[rule].length() == 4 && (year >= 2010 && year <= 2020))
                    continue;
                else return false;
            }
            else if (rule == "eyr"){
                int year = safetoi(rules[rule]);
                if (rules[rule].length() == 4 && (year >= 2020 && year <= 2030))
                    continue;
                else return false;
            }
            else if (rule == "hgt"){
                string value_string = rules[rule];
                int length = value_string.length();
                string unit = value_string.substr(length-2, length);
                // cout << value_string << endl;
                string tempvalue = value_string.substr(0, length-2);
                int value = safetoi(tempvalue);
                if ((unit == "cm" && (value >= 150 && value <= 193)) ||
                    (unit == "in" && (value >= 59  && value <= 76 )) )
                    continue;
                else return false;
            }
            else if (rule == "hcl"){
                if (check_hex(rules[rule]))
                    continue;
                else return false;
            }
            else if (rule == "ecl"){
                if (eyecolors.find(rules[rule]) != eyecolors.end())
                    continue;
                else return false;
            }
            else if (rule == "pid"){
                if (rules[rule].length() == 9 && is_digits(rules[rule]))
                    continue;
                else return false;
            }
            else if (rule == "cid") continue;
            else return false;
        }
    }
    return true;
}


int main() {
    ifstream input_data("inputs/q4.txt");
    string input((istreambuf_iterator<char>(input_data)), istreambuf_iterator<char>());

    int count = 0;

    size_t current = 0;
    size_t next = string::npos;
    do {
        next = input.find("\n\r", current);
        string passport = input.substr(current, next-current);
        if (strict_verify(passport)) count++;
        current = next + 2;
    } while (next != string::npos);
    cout << count << endl;
    return 0;
}