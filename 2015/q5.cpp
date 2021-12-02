#include <iostream>
#include <bits/stdc++.h>
using namespace std;

bool is_vowel(char c) {
	switch (c) {
		case 'a':
		case 'e':
		case 'i':
		case 'o':
		case 'u':
			return true;
	}
	return false;
}

bool is_nice(string line) {
	int vowel = 0;
	bool consecutive = 0;
	for(int i=0; i < line.size() - 1; i++) {
		// ab, cd, pq, or xy
		if(
			(line[i] == 'a' && line[i+1] == 'b') ||
			(line[i] == 'c' && line[i+1] == 'd') ||
			(line[i] == 'p' && line[i+1] == 'q') ||
			(line[i] == 'x' && line[i+1] == 'y')
		)
			return false;
		
		// vowels
		vowel += is_vowel(line[i]);

		// consecutive
		consecutive |= (line[i] == line[i+1]);
	}
	vowel += is_vowel(line[line.size() - 1]);
	return vowel > 2 && consecutive;
}

// vjqkhkhjlmvpwkud
// 0123456789012345
bool matching_pair(string line) {
	unordered_set<string> set;
	int idx, len = line.size();
	string p1, p2, back = "";
	bool pair_match = false, triple = false;
	int half = len / 2 - !(len & 1);
	for(int i=0; i < half; i++) {
		set.insert(back);
		idx = 2 * i;
		p1 = string(1, line[idx]) + line[idx + 1];
		p2 = string(1, line[idx + 1]) + line[idx + 2];
		if (set.find(p1) != set.end() || set.find(p2) != set.end()) {
			return true;
		}
		else {
			set.insert(p1);
			back = p2;
		}
	}
	p1 = string(1, line[line.size() - 2]) + line[line.size() - 1];
	if (!(len & 1) && set.find(p1) != set.end())
		return true;
	return false;
}

bool has_triple(string line) {
	for(int i = 0; i < line.size() - 2; i++) {
		if (line[i] == line[i+2]) return true;
	}
	return false;
}

int q1(ifstream &input_data) {
	string input;
	int count = 0;
	while (getline(input_data, input)) {
		if (is_nice(input))
			count++;
	}
	return count;
}

int q2(ifstream &input_data) {
	string input;
	int count = 0;
	while (getline(input_data, input)) {
		if (matching_pair(input) && has_triple(input))
			count++;
	}
	return count;
}

int main()
{
    ifstream input_data("inputs/q5.txt");
    auto result1 = q1(input_data);
    cout << result1 << endl;
		input_data.clear();
		input_data.seekg(0, ios::beg);
    auto result2 = q2(input_data);
    cout << result2 << endl;

    return 0;
}