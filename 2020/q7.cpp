#include<iostream>
#include <bits/stdc++.h>
using namespace std;
typedef unordered_map<string, int> graph_list;
typedef unordered_map<string, graph_list> graph;

regex bag_subrule("([0-9]+) (\\w+ \\w+) bag[s]?[,.]");
regex bag_subrules("([0-9]+ \\w+ \\w+ bag[s]?[,.])+");

tuple<int, string> parse_subrule(string subrule) {
    int count = 0;
    string color_key = "hello";
    smatch match;
    regex_match(subrule, match, bag_subrule);
    return tuple<int, string>{stoi(match.str(1)), match.str(2)};
}

void gold_source(graph &color_graph, string &rule) {
    string color_key = rule.substr(0, rule.find(" bags"));
    graph_list color_list;

    if (rule.find("no other bags.") == string::npos) {
        smatch match;
        string::const_iterator search_start(rule.cbegin());
        while(regex_search(search_start, rule.cend(), match, bag_subrules)){ 
            auto result = parse_subrule(match.str(1));
            search_start = match.suffix().first;
            color_list[get<1>(result)] = get<0>(result);
        }
    }
    if (color_graph.find(color_key) == color_graph.end()) 
        color_graph[color_key] = color_list;
}

int gold_bag(graph &color_graph) {
    queue<string> q;
    unordered_set<string> possibilities;
    string key_entity;
    q.push("shiny gold");
    do {
        key_entity = q.front();
        for(auto entity : color_graph) {
            if (entity.second.find(key_entity) != entity.second.end()) {
                q.push(entity.first);
                possibilities.insert(entity.first);
            }
        }
        q.pop();
    } while(!q.empty());

    return possibilities.size();
}

int gold_bag_volume(graph &color_graph, string current) {
        int value = 0;
        for(auto entity : color_graph[current])
            value += entity.second * (1 + gold_bag_volume(color_graph, entity.first));
        return value;
}

int main() {
    ifstream input_data("inputs/q7.txt");
    string input;
    graph color_graph;
    while(getline(input_data, input))
        gold_source(color_graph, input);
    
    // cout << gold_bag(color_graph) << endl;
    cout << gold_bag_volume(color_graph, "shiny gold");
    return 0;
}