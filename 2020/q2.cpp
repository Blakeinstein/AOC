#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

regex q1("([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-z]*)[\\S\\s]*");

bool corrupt_password(string input) {
    // parse input

    smatch match;
    if (regex_match(input, match, q1) == true) {
        int low   = stoi(match.str(1)),
            high  = stoi(match.str(2)),
            count = 0;
        char character = match.str(3)[0];

        for ( char x : match.str(4) ) {
            if ( x == character )
                count++;
            if ( count > high)
                return false;
        }
        if ( count < low )
            return false;

        return true;
    } 
    else 
        cout << "Failed to match: " << input << endl;

    return false;
}

bool corrupt_password2(string input) {
    smatch match;
    if (regex_match(input, match, q1) == true) {
        int first   = stoi(match.str(1)),
            second  = stoi(match.str(2));
        char character = match.str(3)[0];

        if (match.str(4)[first - 1] == character ^ match.str(4)[second - 1] == character)
            return true;

    } 
    else 
        cout << "Failed to match: " << input << endl;

    return false;
}

int main() {
    ifstream input_data("inputs/q2.txt");
    string input;
    int count = 0;
    while(getline(input_data, input)) 
        if (corrupt_password2(input))
            count++;
        // cout << count++ << ": " << input << endl;
    cout << count << endl;
    return 0;
}