#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

int yes_count(string &group_answers){
    istringstream input_stream(group_answers);
    string person_answers;
    unordered_set<char> questions;

    while(input_stream >> person_answers) {
        for(char ques : person_answers)
            questions.insert(ques);
    }

    return questions.size();
}

int common_yes(string &group_answers){
    istringstream input_stream(group_answers);
    string person_answers;
    input_stream >> person_answers;
    unordered_set<char> questions(person_answers.begin(), person_answers.end());

    while(input_stream >> person_answers) {
        unordered_set<char> tempset;
        for(char ques : person_answers)
            if (questions.find(ques) != questions.end()) tempset.insert(ques);

        questions = tempset;
    }
    // ab
    // ac
    // a b

    return questions.size();
}

int main() {
    ifstream input_data("inputs/q6.txt");
    string input((istreambuf_iterator<char>(input_data)), istreambuf_iterator<char>());
    int count = 0;

    size_t current = 0;
    size_t next = string::npos;
    do {
        next = input.find("\n\r", current);
        string group_answers = input.substr(current, next-current);
        // count += yes_count(group_answers);
        count += common_yes(group_answers);
        current = next + 2;
    } while (next != string::npos);
    cout << count << endl;
        
    return 0;
}