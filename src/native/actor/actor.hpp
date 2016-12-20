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
    thread t;
    condition_variable cv;
    mutex mx;
    bool tFinished;

public:
    Actor() : tFinished(false) { }

    ~Actor() { }

    Actor& operator=(const Actor& rhs) {
        if (&rhs == this)
            return *this;

        tFinished = rhs.tFinished;

        return *this;
    }

    virtual void receive(Message* msg) = 0;

    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};


#endif  // __ACTOR__
