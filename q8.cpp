#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef tuple<string, int> op;
typedef vector<op> mem;

void build_instruction_memory(mem &memory, string operation, string operand) {
    memory.push_back(op{operation, stoi(operand)});
}

int detect_loop(mem &memory){
    int ir = 0, ax = 0, len = memory.size();
    vector<bool> visited(len, false);

    while(!visited[ir]) {
        op instruction = memory[ir];
        string operation = get<0>(instruction);
        int operand = get<1>(instruction);
        if (ir > len) break;
        visited[ir] = true;
        ir++;
        if(operation == "nop")
            continue;
        else if(operation == "acc")
            ax += operand;
        else
            ir += operand - 1;
    }
    return ax;
}
tuple<bool, int> test(mem &memory, int flip_instruction){
    int ir = 0, ax = 0, len = memory.size();
    vector<bool> visited(len, false);

    while(!visited[ir]) {
        op instruction = memory[ir];
        string operation = get<0>(instruction);
        if (ir == flip_instruction) operation = operation == "nop"? "jmp" : "nop";
        int operand = get<1>(instruction);
        if (ir >= len) break;
        visited[ir] = true;
        ir++;
        if(operation == "nop")
            continue;
        else if(operation == "acc")
            ax += operand;
        else
            ir += operand - 1;
    }
    return tuple<bool, int> {(ir >= len), ax};
}

int fix_loop(mem &memory) {
    queue<int> flip_queue;
    int len = memory.size();
    vector<bool> visited(len, false);
    for(int i = 0; i < len; i++) {
        op instruction = memory[i];
        string operation = get<0>(instruction);
        int operand = get<1>(instruction);
        if(operation == "nop" && operand > 0) flip_queue.push(i);
        else if(operation == "jmp" && operand < 0) flip_queue.push(i);
    }

    while(!flip_queue.empty()) {
        auto result = test(memory, flip_queue.front());
        if (get<0>(result)) return get<1>(result);
        flip_queue.pop();
    }

    return 0;
}

int main() {
    ifstream input_data("inputs/q8.txt");
    string operation, operand;
    mem memory;
    while(input_data >> operation && input_data >> operand)
        build_instruction_memory(memory, operation, operand);
    
    // cout << detect_loop(memory) << endl;
    cout << fix_loop(memory) << endl;
    return 0;
}