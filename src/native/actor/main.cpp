#include <unordered_map>
#include <vector>
#include <iostream>

#include "actor.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication

void add(int a, int b) {
    cout <<  a+b << endl;
}

// this main is more of a prototype at this point
// we need to store Actors so that they can exchange messages
int main() {
    patFxnPtr p;
    p["addTwo"] = &add;
    Actor a(p);

    vector<pair<int, int>> v1;
    v1.push_back(make_pair(1, 2));
    Message msg("addTwo", v1);
    a.receive(msg);

    vector<pair<int, int>> v2;
    v2.push_back(make_pair(3, 4));
    Message msg2("addTwo", v2);
    a.receive(msg2);

    return 0;
}