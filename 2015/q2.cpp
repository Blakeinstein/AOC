#include <iostream>
#include <bits/stdc++.h>
using namespace std;

typedef tuple<int, int, int> dim;
static regex dimension("([0-9]+)x([0-9]+)x([0-9]+)");

long long int q1(vector<dim> dims)
{
    long long int length = 0;
    for (auto gift : dims)
    {
        int l = get<0>(gift),
            w = get<1>(gift),
            h = get<2>(gift);
        length += 2 * (l * w + w * h + h * l);
        if (l > w)
        {
            if (h > l)
                length += l * w;
            else
                length += h * w;
        }
        else
        {
            if (h > w)
                length += l * w;
            else
                length += h * l;
        }
    }

    return length;
}

long long int q2(vector<dim> dims)
{
    long long int length = 0;
    for (auto gift : dims)
    {
        int l = get<0>(gift),
            w = get<1>(gift),
            h = get<2>(gift);
        length += l * w * h;
        if (l > w)
        {
            if (h > l)
                length += 2 * (l + w);
            else
                length += 2 * (h + w);
        }
        else
        {
            if (h > w)
                length += 2 * (l + w);
            else
                length += 2 * (h + l);
        }
    }
    return length;
}

int main()
{
    ifstream input_data("inputs/q2.txt");
    string input;
    vector<dim> dims;
    smatch match;
    while (getline(input_data, input))
    {
        if (regex_search(input, match, dimension))
        {
            // cout << stoi(match.str(1)) << " " << stoi(match.str(2)) << " " << stoi(match.str(3)) << endl;
            dims.push_back(make_tuple(stoi(match.str(1)), stoi(match.str(2)), stoi(match.str(3))));
        }
        else
        {
            cout << input << endl;
            return -1;
        }
    }

    // auto result1 = q1(dims);
    // cout << result1 << endl;
    auto result2 = q2(dims);
    cout << result2 << endl;

    return 0;
}