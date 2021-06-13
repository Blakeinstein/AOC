#include<iostream>
#include <bits/stdc++.h> 
using namespace std;
typedef vector<string> vs;
typedef long long int llt;

template<typename T>
T yield(stack<T> &q){
    T rv = q.top();
    q.pop();
    return rv;
}
template<typename T>
void printstack(stack<T> &s){
    if (s.empty()) return;
    T t = yield(s);
    cout << t << " ";
    printstack(s);
    s.push(t);
}
template<typename T>
stack<T> reverse(stack<T> &s){
    stack<T> news;
    while(!s.empty()){
        news.push(s.top());
        s.pop();
    }
    return news;
}

llt ques1(vs &expressions){
    llt sum = 0;
    string token;

    for(auto expr : expressions){
        stack<llt> val;
        stack<char> ops;
        for(int i=0; i<expr.length(); i++){
            if (expr[i] == ' ') continue;
            if (expr[i] == '(') ops.push('(');
            else if (isdigit(expr[i])) val.push(expr[i] - '0');
            else if (expr[i] == ')') {
                queue<char> temp;
                queue<llt> itemp;
                while(!ops.empty() && ops.top() != '(')
                    temp.push(yield(ops));
                for(int i=0; i<=temp.size(); i++)
                    itemp.push(yield(val));
                while(!temp.empty()){
                    ops.push(temp.front());
                    temp.pop();
                }
                while(!itemp.empty()){
                    val.push(itemp.front());
                    itemp.pop();
                }
                while(!ops.empty() && ops.top() != '('){
                    llt op2 = yield(val);
                    llt op1 = yield(val);
                    char op = yield(ops);
                    if (op == '*')
                        op1 *= op2;
                    else if (op == '+')
                        op1 += op2;
                    val.push(op1);
                }
                if (!ops.empty())
                    ops.pop();
            } else {
                if (expr[i] == '*' || expr[i] == '+')
                    ops.push(expr[i]);
            }
        }
        ops = reverse(ops);
        val = reverse(val);
        while(!ops.empty()){
            llt op2 = yield(val);
            llt op1 = yield(val);
            char op = yield(ops);
            if (op == '*')
                op1 *= op2;
            else if (op == '+')
                op1 += op2;
            val.push(op1);
        }
        sum += val.top();
    }
    return sum;
}

int precedence(char op){
    if (op == '+')
        return 2;
    else
        return (op == '*');
}

llt ques2(vs &expressions){
    llt sum = 0;
    string token;
    
    for(auto expr : expressions){
        stack<llt> val;
        stack<char> ops;
        for(int i=0; i<expr.length(); i++){
            if (expr[i] == ' ') continue;
            if (expr[i] == '(') ops.push('(');
            else if (isdigit(expr[i])) val.push(expr[i] - '0');
            else if (expr[i] == ')') {
                while(!ops.empty() && ops.top() != '('){
                    char op = yield(ops);
                    llt op1, op2;
                    op1 = yield(val);
                    while(!ops.empty() && precedence(ops.top()) >= precedence(op)){
                        op2 = yield(val);
                        char nop = yield(ops);
                        if (nop == '*')
                            op1 *= op2;
                        else if (nop == '+')
                            op1 += op2;
                    }
                    op2 = yield(val);
                    if (op == '*')
                        op1 *= op2;
                    else if (op == '+')
                        op1 += op2;
                    val.push(op1);
                }
                if (!ops.empty())
                    ops.pop();
            }
            else if (expr[i] == '*' || expr[i] == '+'){
                
                while(!ops.empty() && precedence(ops.top()) >= precedence(expr[i])){
                    llt op2 = yield(val);
                    llt op1 = yield(val);
                    char op = yield(ops);
                    
                    if (op == '*')
                        op1 *= op2;
                    else if (op == '+')
                        op1 += op2;
                    val.push(op1);
                }
                
                ops.push(expr[i]);
            }
        }
        while(!ops.empty()){
            char op = yield(ops);
            llt op1, op2;
            op1 = yield(val);
            while(!ops.empty() && precedence(ops.top()) >= precedence(op)){
                op2 = yield(val);
                char nop = yield(ops);
                if (nop == '*')
                    op1 *= op2;
                else if (nop == '+')
                    op1 += op2;
            }
            op2 = yield(val);
            if (op == '*')
                op1 *= op2;
            else if (op == '+')
                op1 += op2;
            val.push(op1);
        }
        sum += val.top();
    }
    return sum;
}

int main() {
    ifstream input_data("inputs/q18.txt");
    string input;
    vs expressions;

    while(getline(input_data, input))
        expressions.push_back(input);

    // auto result1 = ques1(expressions);
    // cout << "part1) " << result1 << endl;
    auto result2 = ques2(expressions);
    cout << "part2) " << result2 << endl;

    return 0;
}