#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef vector<int> ticket;
typedef long long int llt;
typedef tuple<int, int> range;

regex rule("([\\w ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"),
      number("([0-9]+)![0-9]");

llt error_rate(vector<tuple<int, int, int, int> > &ranges, vector<ticket> &nearby_tickets){
    vector<range> bounds;
    stack<range> range_stack;
    for (auto x : ranges){
        bounds.push_back(tuple<int, int>{get<0>(x), get<1>(x)});
        bounds.push_back(tuple<int, int>{get<2>(x), get<3>(x)});
    }
    sort(bounds.begin(), bounds.end());
    range_stack.push(bounds[0]);

    for(int i=1; i<bounds.size(); i++){
        range top = range_stack.top();
        if(get<1>(top) + 1 < get<0>(bounds[i]))
            range_stack.push(bounds[i]);
        else if(get<1>(top) < get<1>(bounds[i])){
            get<1>(top) = get<1>(bounds[i]);
            range_stack.pop();
            range_stack.push(top);
        }
    }
    bounds.clear();
    while(!range_stack.empty()){
        bounds.push_back(range_stack.top());
        range_stack.pop();
    }

    llt error_rate = 0;
    for(auto ticket : nearby_tickets){
        for(auto value : ticket){
            for(auto range : bounds){
                if (get<0>(range) <= value && get<1>(range) >= value){
                    goto forelse;
                }
            }
            error_rate += value;
            forelse:
            continue;
        }
    }

    return error_rate;
}

llt ticket_data(vector<tuple<int, int, int, int> > &ranges, vector<ticket> &nearby_tickets,
    ticket &my_ticket, vector<int> &valid_fields){
    llt ans = 1;
    unordered_set<int> checks;
    int x1, y1, x2, y2;
    vector<set<int> > columns, set_ranges;
    for(int i=0; i<ranges.size(); i++){
        set<int> new_ticket, new_range;
        for(auto x: nearby_tickets) new_ticket.insert(x[i]);
        columns.push_back(new_ticket);
        tie(x1, y1, x2, y2) = ranges[i];
        for (int i=x1; i<=y1; i++) new_range.insert(i);
        for (int i=x2; i<=y2; i++) new_range.insert(i);
        set_ranges.push_back(new_range);
    }

    for(auto x : valid_fields){
        for(int i=0; i<set_ranges.size(); i++){
            if (checks.find(i) != checks.end()) continue;
            if (includes(set_ranges[x].begin(), set_ranges[x].end(),
                columns[i].begin(), columns[i].end())){
                cout << i << endl;
                ans *= my_ticket[i];
                checks.insert(i);
                break;
            }
        }
    }
    
    return ans;
}
int main() {
    ifstream input_data("inputs/q16.txt");
    string input, substr;
    smatch match;
    vector<tuple<int, int, int, int> > ranges;
    ticket my_ticket;
    vector<ticket> nearby_tickets;
    vector<int> valid_fields;
    int i = 0;
    while(getline(input_data, input)){
        if (regex_search(input, match, rule)){
            if (match.str(1).rfind("departure ", 0)==0) valid_fields.push_back(i);
            ranges.push_back(tuple<int, int, int, int>{
                stoi(match.str(2)),
                stoi(match.str(3)),
                stoi(match.str(4)),
                stoi(match.str(5)),
            });
            i++;
        } else 
            break;
    }

    getline(input_data, input);
    getline(input_data, input);
    stringstream ss(input);
    while(ss.good()){
        getline( ss, substr, ',' );
        my_ticket.push_back(stoi(substr));
    }
    getline(input_data, input);
    getline(input_data, input);
    while(getline(input_data, input)){
        stringstream ss(input);
        ticket new_ticket;
        while(ss.good()){
            getline( ss, substr, ',' );
            new_ticket.push_back(stoi(substr));
        }
        nearby_tickets.push_back(new_ticket);
    }

    auto result1 = error_rate(ranges, nearby_tickets);
    cout << "part1) " << result1 << endl;
    auto result2 = ticket_data(ranges, nearby_tickets, my_ticket, valid_fields);
    cout << "part2) " << result2 << endl;

    return 0;
}