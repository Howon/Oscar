#ifndef __POOL__
#define __POOL__

#include <vector>
#include <queue>

#include <condition_variable>
#include <mutex>
#include <thread>

#include "actor.hpp"
#include "pingpong.hpp"
using namespace std;

class Pool {
    vector<Actor *> workers;
    queue<Message *> tasks;

    condition_variable cv;
    mutex mx;
    thread t;

    atomic<int> counter;
    bool tFinished;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (!tFinished && tasks.empty())
                cv.wait(lck);

            if (tFinished && tasks.empty())
                return;

            int i = (counter++) % workers.size();
            Message* msg = tasks.front();
            tasks.pop();

            workers[i]->receive(msg);
        }
    }

public:
    Pool(Actor *actor, int cap) : tFinished(false), counter(0) {
        for (int i = 0; i < cap; ++i)
            // todo: might need to copy ... but it's a parent class
            workers.push_back(actor);

        t = thread([=] { consume(); });
    }

    ~Pool() {
        {
            unique_lock<mutex> lck(mx);
            tFinished = true;
        }

        cv.notify_all();
        for (Actor *a : workers)
            a->t.join();
    }

    void receive(Message* msg) {
        {
            unique_lock<mutex> lck(mx);
            tasks.push(msg);
        }

        cv.notify_one();
    }

};


#endif //__POOL__
