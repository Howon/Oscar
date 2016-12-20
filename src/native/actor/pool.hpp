#ifndef __POOL__
#define __POOL__

#include <vector>
#include <queue>

#include <condition_variable>
#include <mutex>
#include <thread>

#include "actor.hpp"
#include "pingpong.hpp"


//template <typename T>
class Pool {
    thread t;
    condition_variable cv;
    mutex mx;
    bool tFinished;

    vector<Ping> pings;
    vector<Pong> pongs;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (q.empty())
                cv.wait(lck);

            int i = (count++) % pings.size();
            Message* msg = q.front();
            q.pop();

            pings[i].receive(msg);
        }
    }

public:
    // circular buffer
    vector<Actor*> cbuff;
    int count;
    int cap;

    queue<Message*> q;

    Pool(int cap=3) : count(0), cap(cap)
    {
        pings.resize(cap);
        pongs.resize(cap);

        // assign things
        for (int i = 0; i < pings.size(); ++i) {
            pings[i].setmaxTurns(1);
            pings[i].setPong(&pongs[i]);
        }

        t = thread([=] { consume(); });
    }

    ~Pool() {
        t.join();
    }

//    Pool(const vector<Actor*>& actors) : count(0) {
//        cbuff = actors;
//    }

//    void receive(const vector<Message*>& msgs) {
//        for (int i = 0; i < msgs.size(); ++i)
//            int ndx = (count++) % cbuff.size();
//            cbuff[ndx]->receive(msgs[i]);
//        }
//    }

    void receive(Message* msg) {
        {
            unique_lock<mutex> lck(mx);
            q.push(msg);
        }

        cv.notify_one();
    }

};


#endif //__POOL__
