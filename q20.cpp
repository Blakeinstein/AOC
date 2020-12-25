#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef vector<string> vs;
typedef long long int llt;

const int width = 10, height = 10;

struct tile{
    int key;
    vs mat;
    void flipH(){
        for (int i = 0; i < height; ++i)
            for (int l = 0, r = width - 1; l < r; ++l, --r)
                swap(mat[i][l], mat[i][r]);
    }
    void rotate(){
        vs newmat = mat;
        for(int i = 0; i<height; i++){
            for(int j = 0; j<width; j++){
                int newi = height - 1 - j, newj = i;
                newmat[newi][newj] = mat[i][j];
            }
        }
        mat.swap(newmat);
    }
};
vector<tile *> tiles;
vector<tuple<int, int> > idx;
int keym[20][20];
vector<string> img, sea_monster;

bool match_sides(int i, int j){
    if (i > 0) {
        for (int k = 0; k < width; k++) {
            if (tiles[keym[i][j]]->mat[0][k] != tiles[keym[i - 1][j]]->mat[height - 1][k]) return false;
        }
    }
    if (j > 0){
        for (int k = 0; k < height; k++) {
            if (tiles[keym[i][j]]->mat[k][0] != tiles[keym[i][j - 1]]->mat[k][width - 1]) return false;
        }
    }
    return true;
}

llt bkt(int i, int j, int side){
    if (i>=side) {
        llt ans = 1;
        ans *= tiles[keym[0][0]]->key;
        ans *= tiles[keym[0][side-1]]->key;
        ans *= tiles[keym[side-1][0]]->key;
        ans *= tiles[keym[side-1][side-1]]->key;
        return ans;
    }
    if (j>=side)
        return bkt(i+1, 0, side);
    for(int k=0; k<tiles.size(); k++){
        if (get<0>(idx[k]) >= 0) continue;
        idx[k] = tuple<int, int>{i, j};
        keym[i][j] = k;
        for(int x=0; x < 2; x++, tiles[k]->flipH()){
            for(int y=0; y < 4; y++, tiles[k]->rotate()){
                if(!match_sides(i, j)) continue;
                auto ans = bkt(i, j+1, side);
                if (ans < 0) continue;
                return ans;
            }
        }
        idx[k] = tuple<int, int>{-1, -1};
    }
    return -1;
}

llt ques1(){
    int side = sqrt(tiles.size());
    idx.resize(tiles.size(), tuple<int, int>{-1, -1});
    return bkt(0, 0, side);
}

llt ques2(){
    int side = sqrt(tiles.size());
    return bkt(0, 0, side);
}

int main() {
    ifstream input_data("inputs/q20.txt");
    string input((istreambuf_iterator<char>(input_data)), istreambuf_iterator<char>()), tiledata, temp;

    size_t current = 0, data_start = 0, key_start = 0;
    size_t next = string::npos;
    do {
        next = input.find("\n\r", current);
        tiledata = input.substr(current+1, next-current-1);
        data_start = tiledata.find(":", 0);
        key_start = tiledata.find(" ", 0);

        int key = stoi(tiledata.substr(key_start, data_start-3));
        istringstream data(tiledata.substr(data_start+3, next-current));
        vs keydata;
        while(data >> temp){
            keydata.push_back(temp);
        }
        tiles.push_back(new tile);
        tiles.back()->key = key;
        tiles.back()->mat = keydata;
        current = next + 2;
    } while (next != string::npos);
    
    auto result1 = ques1();
    cout << "part1) " << result1 << endl;
    
    auto result2 = ques2();
    cout << "part2) " << result2 << endl;

    return 0;
}