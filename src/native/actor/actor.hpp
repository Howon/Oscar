#ifndef __ACTOR__
#define __ACTOR__

#include "iostream"
#include <cstdio>
#include <queue>
#include <string>
#include <unordered_map>

#include <condition_variable>
#include <mutex>
#include <thread>

#include "message.hpp"

using namespace std;

class Actor {
public:
    std::thread t;
    condition_variable cv;
    mutex mx;
    bool tFinished;

public:
    Actor() : tFinished(false) { }

    ~Actor() { }

    virtual void receive(Message* const msg) = 0;

    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};


#endif  // __ACTOR__
