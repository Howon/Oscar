#ifndef __ACTOR__
#define __ACTOR__

#include <cstdio>
#include <queue>
#include <unordered_map>

#include <condition_variable>
#include <mutex>
#include <thread>

#include "message.hpp"
using namespace std;

// todo: need to have variable arg list
typedef void (*fxnPtr)(int, int);

typedef unordered_map<string, fxnPtr> patFxnPtr;

class Actor {
    patFxnPtr patterns;
    queue<Message> q;

    // synchronization stuff
    std::thread t;
    condition_variable cv;
    mutex mx;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (q.empty())
                cv.wait(lck);

            while (!q.empty()) {
                // process current message
                const Message &msg = q.front();
                auto itr = patterns.find(msg.name);
                // matched pattern
                if (itr != patterns.end())
                    (*itr->second)(msg.formals[0].first, msg.formals[0].second);

                // remove message from queue
                q.pop();
            }
        }
    }

public:
    Actor(const unordered_map<string, fxnPtr>& p) : patterns(p) {
        t = std::thread([=] { consume(); });
    }

    ~Actor() {
        t.join();
    }

    void receive(const Message& msg) {
        unique_lock<mutex> lck(mx);
        q.push(msg);
        cv.notify_one();
    }

    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};

#endif  // __ACTOR__