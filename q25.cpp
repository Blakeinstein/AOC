#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef unsigned long long int llt;

llt ques1(llt card, llt door){
    llt key = 1, loopsize = 0;
    const llt mod = 20201227;
    while(key!=card){
        key = (key * 7) % mod;
        loopsize++;
    }
    key = 1;
    for(int i=0; i<loopsize; i++)
        key = (key * door) % mod;
    return key;
}

int main() {
    auto result1 = ques1(16915772, 18447943);
    cout << "part1) " << result1 << endl;
        
    return 0;
}