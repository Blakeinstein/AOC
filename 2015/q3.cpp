
#include <iostream>
#include <bits/stdc++.h>
using namespace std;

void step(char dir, int &x, int &y, unordered_set<string> &visited) {
	switch(dir) {
		case '^':
			y++;
			break;
		case '>':
			x++;
			break;
		case '<':
			x--;
			break;
		case 'v':
			y--;
			break;
	}
	visited.insert(to_string(x) + to_string(y));
}

int q1(string input) {
	unordered_set<string> visited;
	int x = 0, y = 0;
	visited.insert(to_string(x) + to_string(y));
	for(auto c : input) {
		step(c, x, y, visited);	
	}
	return visited.size();
}

long int q2(string input) {
	unordered_set<string> visited;
	int x1 = 0, y1 = 0, x2 = 0, y2 = 0;
	visited.insert(to_string(0) + to_string(0));
	bool is_robot = false;
	for(auto c : input) {
		if (is_robot)
			step(c, x1, y1, visited);
		else
			step(c, x2, y2, visited);
		is_robot = !is_robot;
	}
	return visited.size();
}

int main()
{
    ifstream input_data("inputs/q3.txt");
		stringstream stream;
		stream << input_data.rdbuf();
    string input = stream.str();

    // auto result1 = q1(input);
    // cout << result1 << endl;
    auto result2 = q2(input);
    cout << result2 << endl;

    return 0;
}