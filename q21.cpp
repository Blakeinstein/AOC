#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef vector<string> vs;
typedef unordered_set<string> us;

template<typename T>
unordered_set<T> set_intersec(unordered_set<T> &first, unordered_set<T> &second){
    unordered_set<T> result;
    for(auto x : first)
        if (second.find(x) != second.end())
            result.insert(x);
    for(auto x : second)
        if (first.find(x) != first.end())
            result.insert(x);
    return result;
}

int ques1(vs &lines){
    us free;
    unordered_map<string, us> allergen; 
    vector<pair<us, us> > input;
    string temp;
    for(auto x: lines){
        us ing, allg;
        istringstream line(x);
        while(line >> temp){
            if (temp[0] == '(') break;
            ing.insert(temp);
        }
        while(line >> temp){
            temp.pop_back();
            allg.insert(temp);
        }
        input.push_back(make_pair(ing, allg));
    }

    for (auto x: input){
        for(auto y : x.second){
            if (allergen.find(y) == allergen.end()){
                allergen[y].insert(x.first.begin(), x.first.end());
            } else {
                allergen[y] = set_intersec(allergen[y], x.first);
            }
        }
    }
    for (auto x : allergen){
        free.insert(x.second.begin(), x.second.end());
    }

    int count = 0;
    for(auto x: input){
        for(auto y: x.first){
            if (free.find(y) == free.end()) count++;
        }
    }

    return count;
}

string ques2(vs &lines){
    unordered_map<string, us> allergen; 
    map<string, string> map;
    vector<pair<us, us> > input;
    string temp;
    for(auto x: lines){
        us ing, allg;
        istringstream line(x);
        while(line >> temp){
            if (temp[0] == '(') break;
            ing.insert(temp);
        }
        while(line >> temp){
            temp.pop_back();
            allg.insert(temp);
        }
        input.push_back(make_pair(ing, allg));
    }

    for (auto x: input){
        for(auto y : x.second){
            if (allergen.find(y) == allergen.end()){
                allergen[y].insert(x.first.begin(), x.first.end());
            } else {
                allergen[y] = set_intersec(allergen[y], x.first);
            }
        }
    }

    int test = 2;
    while(allergen.size()>0){
        vs other;
        us free;
        for(auto x: allergen){
            if (x.second.size() == 1){
                for (auto y: x.second){
                    map[x.first] = y;
                    free.insert(y);
                }
                other.push_back(x.first);
            }
        }
        for(auto x: other){
            allergen.erase(x);
        }
        for(auto &x: allergen){
            us temp;
            for (auto y : x.second){
                if(free.find(y) == free.end()){
                    temp.insert(y);
                }
            }
            x.second = temp;
        }
    }

    temp = "";
    for (auto x: map){
        temp += x.second+",";
    }
    temp.pop_back();
    return temp;
}

int main() {
    ifstream input_data("inputs/q21.txt");
    string input;
    vs input_lines;
    while(getline(input_data, input))
        input_lines.push_back(input);

    auto result1 = ques1(input_lines);
    cout << "part1) " << result1 << endl;
    
    auto result2 = ques2(input_lines);
    cout << "part2) " << result2 << endl;

    return 0;
}