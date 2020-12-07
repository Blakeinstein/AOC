#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

int trees(vector<string> terrain, int right, int down) {
    int count = 0, x = 0, y = 0;
    int width = terrain[0].length() - 1;
    int height = terrain.size();

    while(y < height) {
        if (terrain[y][x] == '#')
            count++;
        
        y += down;
        x = (x + right) % width;

    }
    
    return count;
}

int main() {
    ifstream input_data("inputs/q3.txt");
    string input;
    vector<string> terrain;

    while(getline(input_data, input))
        terrain.push_back(input);

    // cout << trees(terrain, 3, 1) << endl;
    long long int ans;
    ans = trees(terrain, 1, 1)
        * trees(terrain, 3, 1)
        * trees(terrain, 5, 1)
        * trees(terrain, 7, 1)
        * trees(terrain, 1, 2);

    cout << ans << endl;


    return 0;
}