#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef long long int llt;
typedef pair<int, int> pii;
#define MP(x, y) make_pair(x, y)

enum direction{
    W,
    E,
    SW,
    NE,
    NW,
    SE
};

template <> struct hash<pii> {
    inline size_t operator()(const pii &v) const {
        hash<int> int_hasher;
        return int_hasher(v.first) ^ int_hasher(v.second);
    }
};

pii operator+(const pii &l,const pii &r) {   
    return {l.first+r.first,l.second+r.second};                                    
}    

llt ques1(vector<vector<direction> > &tile_input){
    unordered_set<pii > blacks;

    for(auto tlist : tile_input){
        int x=0, y=0;
        for(auto dir: tlist){
            switch(dir){
                case E:
                    x += 2;
                    break;
                case SE:
                    x += 1;
                    y -= 1;
                    break;
                case NE:
                    x += 1;
                    y += 1;
                    break;
                case W:
                    x -= 2;
                    break;
                case NW:
                    x -= 1;
                    y += 1;
                    break;
                case SW:
                    x -= 1;
                    y -= 1;
                    break;
                default:
                    cout << "?" << endl;
            }
        }
        pii final = MP(x, y);
        if(blacks.find(final) == blacks.end())
            blacks.insert(final);
        else
            blacks.erase(final);
    }

    return blacks.size();
}

bool adjacent_flip(unordered_map<pii, bool> &grid, pii pos){
    bool curr = grid[pos];
    int n = 0;
    pii delta[] = {MP(1, -1), MP(2, 0), MP(+1, +1), MP(-1, +1), MP(-2, 0), MP(-1, -1)};
    for(pii d : delta){
        if (grid[pos+d])
            n++;
        if (n > 2) break;
    }

    if ((!curr && n==2) || (curr && (n==0 || n > 2))) return true;

    return false;
}

llt ques2(vector<vector<direction> > &tile_input, int days){
    unordered_map<pii, bool> grid;

    for(auto tlist : tile_input){
        int x=0, y=0;
        for(auto dir: tlist){
            switch(dir){
                case E:
                    x += 2;
                    break;
                case SE:
                    x += 1;
                    y -= 1;
                    break;
                case NE:
                    x += 1;
                    y += 1;
                    break;
                case W:
                    x -= 2;
                    break;
                case NW:
                    x -= 1;
                    y += 1;
                    break;
                case SW:
                    x -= 1;
                    y -= 1;
                    break;
                default:
                    cout << "?" << endl;
            }
            pii step = MP(x, y);
            grid[step] = grid[step];
        }
        pii final = MP(x, y);
        if(grid.find(final) == grid.end())
            grid[final] = !grid[final];
    }


    for(int i=0; i<days; i++){
        vector<pii> to_flip;
        for(auto x: grid)
            if (adjacent_flip(grid, x.first)) to_flip.push_back(x.first);
        for(auto x: to_flip)
            grid[x] = !grid[x];
    }
    llt count = 0;
    for (auto x: grid) count += x.second;
    return count;
}

int main() {
    ifstream input_data("inputs/q24.txt");
    string input;
    vector<vector<direction> > tile_input;
    while(getline(input_data, input)){
        vector<direction> temp;
        bool diag = false, north = false;
        for(char c : input){
            if (c == '\n' || c == '\r') break;
            if (c=='s' || c=='n'){
                diag = true;
                north = c == 'n';
                continue;
            }
            if (diag){
                if (north)
                    temp.push_back(c=='e'? NE : NW);
                else
                    temp.push_back(c=='e'? SE : SW);
            } else
                temp.push_back(c=='e'? E : W);
            diag = false;
        }
        tile_input.push_back(temp);
    }

    auto result1 = ques1(tile_input);
    cout << "part1) " << result1 << endl;
    
    auto result2 = ques2(tile_input, 100);
    cout << "part2) " << result2 << endl;
        
    return 0;
}