#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

enum cell{
    dead,
    alive
};

typedef vector<cell> row;
typedef vector<row> plane;
typedef vector<plane> grid;
typedef vector<grid> hyper_grid;

int alive_count(grid &new_grid){
    int count = 0;
    for(plane p : new_grid)
        for(row r : p)
            for(cell c : r)
                count += c;

    return count;
}

int alive_count(hyper_grid &new_hyper_grid){
    int count = 0;
    for(grid g: new_hyper_grid)
        for(plane p : g)
            for(row r : p)
                for(cell c : r)
                    count += c;

    return count;
}

int count_neighbors(grid &new_grid, int i, int j, int k){
    int count  = 0,
        depth  = new_grid.size(),
        height = new_grid.front().size(),
        width  = new_grid.front().front().size();

    for (int dz : {depth - 1, 0, 1}){
        for (int dy : {height - 1, 0, 1}) {
            for (int dx : {width - 1, 0, 1}) {
                if (dz == 0 && dy == 0 && dx == 0)
                    continue;

                int nz = (i + dz) % depth ,
                    nr = (j + dx) % height,
                    nc = (k + dy) % width ;
                count += new_grid[nz][nr][nc];
            }
        }
    }
    return count;
}

int count_neighbors(hyper_grid &new_hyper_grid, int l, int i, int j, int k){
    int count       = 0,
        complexity  = new_hyper_grid.size(),
        depth       = new_hyper_grid.front().size(),
        height      = new_hyper_grid.front().front().size(),
        width       = new_hyper_grid.front().front().front().size();
    
    for (int dc : {complexity - 1, 0, 1}){
        for (int dz : {depth - 1, 0, 1}){
            for (int dy : {height - 1, 0, 1}) {
                for (int dx : {width - 1, 0, 1}) {
                    if (dc==0 && dz == 0 && dy == 0 && dx == 0)
                        continue;

                    int nc = (l + dc) % complexity,
                        nz = (i + dz) % depth ,
                        ny = (j + dx) % height,
                        nx = (k + dy) % width ;
                    count += new_hyper_grid[nc][nz][ny][nx];
                }
            }
        }
    }    
    return count;
}

void evolve(grid &new_grid, int cycles){
    while(cycles){
        grid temp_grid = new_grid;

        for(int i=0; i<new_grid.size(); i++){
            for(int j=0; j<new_grid.front().size(); j++){
                for(int k=0; k<new_grid.front().front().size(); k++){
                    int count = count_neighbors(new_grid, i, j, k);
                    if (new_grid[i][j][k]){
                        if(count != 2 && count != 3)
                            temp_grid[i][j][k] = dead;
                    } else {
                        if (count == 3)
                            temp_grid[i][j][k] = alive;
                    }
                }
            }
        }
        new_grid = temp_grid;
        cycles--;
    }
}

void evolve(hyper_grid &new_hyper_grid, int cycles){
    int complexity  = new_hyper_grid.size(),
        depth       = new_hyper_grid.front().size(),
        height      = new_hyper_grid.front().front().size(),
        width       = new_hyper_grid.front().front().front().size();

    while(cycles){
        hyper_grid temp_hyper_grid = new_hyper_grid;

        for(int l=0; l<complexity; l++){
            for(int i=0; i<depth; i++){
                for(int j=0; j<height; j++){
                    for(int k=0; k<width; k++){
                        int count = count_neighbors(new_hyper_grid, l, i, j, k);
                        if (new_hyper_grid[l][i][j][k]){
                            if(count != 2 && count != 3)
                                temp_hyper_grid[l][i][j][k] = dead;
                        } else {
                            if (count == 3)
                                temp_hyper_grid[l][i][j][k] = alive;
                        }
                    }
                }
            }
        }
        new_hyper_grid = temp_hyper_grid;
        cycles--;
    }
}

int ques1(vector<string> &input){
    int height = input.size() + 12,
        width  = input.front().size() + 12,
        depth  = 13;

    int ox = width / 2, oy = height / 2, oz = depth/2,
        ix = input.size() / 2, iy = input.front().size() / 2;
    grid new_grid(depth, plane(height, row(width, dead)));

    for(int i=0; i<input.size(); i++){
        for(int j=0; j<input.front().size(); j++){
            if(input[i][j]=='#') new_grid[oz][oy-iy+i][ox-ix+j] = alive;
            else new_grid[oz][oy-iy+i][ox-ix+j] = dead;
        }
    }
    
    evolve(new_grid, 6);

    return alive_count(new_grid);    
}

int ques2(vector<string> &input){
    int height = input.size() + 12,
        width  = input.front().size() + 12,
        depth  = 13;

    int ox = width / 2, oy = height / 2, oz = depth/2,
        ix = input.size() / 2, iy = input.front().size() / 2;
    hyper_grid new_hyper_grid(depth, grid(depth, plane(height, row(width, dead))));

    for(int i=0; i<input.size(); i++){
        for(int j=0; j<input.front().size(); j++){
            if(input[i][j]=='#') new_hyper_grid[oz][oz][oy-iy+i][ox-ix+j] = alive;
            else new_hyper_grid[oz][oz][oy-iy+i][ox-ix+j] = dead;
        }
    }
    
    evolve(new_hyper_grid, 6);

    return alive_count(new_hyper_grid);
}

int main() {
    ifstream input_data("inputs/q17.txt");
    string input;

    vector<string> init_state;
    while(input_data >> input)
        init_state.push_back(input);

    auto result1 = ques1(init_state);
    cout << "part1) " << result1 << endl;
    auto result2 = ques2(init_state);
    cout << "part2) " << result2 << endl;
    return 0;
}