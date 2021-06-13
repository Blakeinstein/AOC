#include<iostream>
#include <bits/stdc++.h> 
using namespace std;

int seat_id(string &pass) {
    if (pass.length() != 10) return 0;
    string  row_id = pass.substr(0, 7),
            col_id = pass.substr(7, 10);

    for (char &b : row_id) b = b=='B'? '1' : '0';
    for (char &b : col_id) b = b=='R'? '1' : '0';
    bitset<7> row(row_id);
    bitset<3> col(col_id);

    return row.to_ulong() * 8 + col.to_ulong();
}

int main() {
    ifstream input_data("inputs/q5.txt");
    string input;
    int max = 0, myseat=0;
    bool seatnumbers[1024] = {false};
    while(input_data >> input) {
        int seatid = seat_id(input);
        if (seatid > max) max = seatid;
        seatnumbers[seatid] = true;
    }

    for (int i=8; i<1016; i++){
        myseat = i;
        if (seatnumbers[i]) continue;
        else if (seatnumbers[i+1] && seatnumbers[i-1]) break;
    }
    // cout << max << endl;
    cout << myseat << endl;
    return 0;
}