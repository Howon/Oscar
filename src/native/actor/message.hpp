#ifndef __MESSAGE__
#define __MESSAGE__

#include <string>
#include <vector>
using namespace std;


class Message {
// todo: make private
public:
    string name;
    // todo: need to be able to pass various types in one list
    vector<pair<int, int>> formals;


public:
    Message(const string& n, const vector<pair<int, int>>& f) : name(n), formals(f)
    { }
};


#endif //__MESSAGE__
