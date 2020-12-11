#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

int adjacent_occupants(vector<string> &seat_map, int row, int col, int width, int height){
    int count = 0;
    vector<int> dh, dw;

    if (row > 0) dh.push_back(-1);
    dh.push_back(0);
    if (row < height - 1) dh.push_back(1);
    if (col > 0) dw.push_back(-1);
    dw.push_back(0);
    if (col < width - 1) dw.push_back(1);

    for (auto dr : dh){
        for (auto dc : dw) {
            if (dr == 0 && dc == 0) continue;
            count += seat_map[row + dr][col + dc] == '#';
        }
    }
    return count;
}

int evolve_and_count(vector<string> seat_map){
    int delta, width = seat_map[0].length(), height = seat_map.size();
    vector<string> next_state;
    do{
        delta = 0;
        next_state.clear();
        for(int i=0; i<height; i++){
            string temp = seat_map[i];
            for(int j=0; j<width; j++){
                if (seat_map[i][j] == '.') continue;
                delta++;
                int seat_count = adjacent_occupants(seat_map, i, j, width, height);
                if (seat_map[i][j] == 'L' && seat_count == 0) temp[j] = '#';
                else if (seat_map[i][j] == '#' && seat_count >= 4) temp[j] = 'L';
                else delta--;
            }
            next_state.push_back(temp);
        }   
        seat_map = next_state;
    }while(delta > 0);

    int count = 0;
    for(auto row : seat_map)
        for(auto i : row) if (i=='#') count++;
    return count;
}

int lookahead_occupants(vector<string> &seat_map, int row, int col, int width, int height){
    int count = 0, dw[3]={-1, 0, 1}, dh[3]={-1, 0, 1};

    for(auto dr : dw){
        for(auto dc : dh){
            if (dr==0 && dc==0) continue;
            int i=row + dr, j=col + dc;
            if (i >= height || j>=width || j<0 || i<0) continue;
            while(i>=0 && j>=0 && i<height && j<width && seat_map[i][j]=='.'){
                i += dr;
                j += dc;
            }
            if (i<0 || j<0 || i>height-1 || j>width-1) continue;
            count += seat_map[i][j] == '#';
        }
    }

    return count;
}

int new_order(vector<string> seat_map){
    int delta, width = seat_map[0].length(), height = seat_map.size();
    vector<string> next_state;
    do{
        delta = 0;
        next_state.clear();
        for(int i=0; i<height; i++){
            string temp = seat_map[i];
            for(int j=0; j<width; j++){
                if (seat_map[i][j] == '.') continue;
                delta++;
                int seat_count = lookahead_occupants(seat_map, i, j, width, height);
                if (seat_map[i][j] == 'L' && seat_count == 0) temp[j] = '#';
                else if (seat_map[i][j] == '#' && seat_count >= 5) temp[j] = 'L';
                else delta--;
            }
            next_state.push_back(temp);
        }   
        seat_map = next_state;
    }while(delta > 0);

    int count = 0;
    for(auto row : seat_map)
        for(auto i : row) if (i=='#') count++;
    return count;
}

int main() {
    ifstream input_data("inputs/q11.txt");
    string input;
    vector<string> seat_map;
    while(input_data >> input)
        seat_map.push_back(input);

    // auto result1 = evolve_and_count(seat_map);
    // cout << result1 << endl;
    auto result2 = new_order(seat_map);
    cout << result2 << endl;
    return 0;
}