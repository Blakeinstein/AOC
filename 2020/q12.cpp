#include<iostream>
#include <bits/stdc++.h>
#define PI 3.14159265
using namespace std;

regex ins("(N|S|E|W|L|R|F)([0-9]+)");

float naive(vector<tuple<char, int> > &guide){
    float posx = 0, posy = 0, dir = 0;
    for(auto ins : guide){
        int mag = get<1>(ins);
        switch(get<0>(ins)){
            case 'N':
                posy += mag;
                break;
            case 'S':
                posy -= mag;
                break;
            case 'E':
                posx += mag;
                break;
            case 'W':
                posx -= mag;
                break;
            case 'L':
                dir += mag * PI / 180;
                break;
            case 'R':
                dir += (- mag) * PI / 180;
                break;
            default:
                posx += round(cos(dir)) * mag;
                posy += round(sin(dir)) * mag; 
                break;  
        }
    }
    return abs(posx) + abs(posy);
}
void rotate(float &wpx, float &wpy, float degrees){
    float angle = degrees * PI / 180, tempx = wpx, tempy = wpy;
    auto sn = sin(angle), cs = cos(angle);
    wpx = tempx * cs - tempy * sn;
    wpy = tempx * sn + tempy * cs;
}

float waypoint(vector<tuple<char, int> > &guide){
    float posx = 0, posy = 0,
          wpx = 10, wpy = 1;
    for(auto ins : guide){
        int mag = get<1>(ins);
        switch(get<0>(ins)){
            case 'N':
                wpy += mag;
                break;
            case 'S':
                wpy -= mag;
                break;
            case 'E':
                wpx += mag;
                break;
            case 'W':
                wpx -= mag;
                break;
            case 'L':
                rotate(wpx, wpy, mag);
                break;
            case 'R':
                rotate(wpx, wpy, -mag);
                break;
            default:
                posx += wpx * mag;
                posy += wpy * mag; 
                break;  
        }
    }
    return abs(posx) + abs(posy);
}
int main() {
    ifstream input_data("inputs/q12.txt");
    string input;
    vector<tuple<char, int> > guide;

    while(input_data >> input){
        smatch match;
        regex_match(input, match, ins);
        guide.push_back(tuple<char, int>{match.str(1)[0], stoi(match.str(2))});
    }

    // auto result1 = naive(guide);
    // cout << result1 << endl;

    auto result2 = waypoint(guide);
    cout << result2 << endl;

    return 0;
}