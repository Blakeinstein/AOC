#include <iostream>
#include <bits/stdc++.h>
using namespace std;

int q1(string input)
{
    int floor = 0;
    for (auto x : input)
    {
        if (x == '(')
            floor++;
        else
            floor--;
    }
    return floor;
}

int q2(string input)
{
    int floor = 0;
    for (int i = 0; i < input.length(); i++)
    {
        if (input[i] == '(')
            floor++;
        else
            floor--;
        if (floor == -1)
        {
            return i + 1;
        }
    }
    return input.length();
}

int main()
{
    ifstream input_data("inputs/q1.txt");
    string input;
    getline(input_data, input);

    // auto result1 = q1(input);
    // cout << result1 << endl;
    auto result2 = q2(input);
    cout << result2 << endl;

    return 0;
}