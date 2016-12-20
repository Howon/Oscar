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
    atomic<int> counter;
    condition_variable cv;
    mutex mx;
    bool tFinished;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (tasks.empty())
                cv.wait(lck);

            int i = (counter++) % workers.size();
            Message* msg = tasks.front();
            tasks.pop();

            workers[i]->receive(msg);
        }
    }

public:
//    Pool(T *actor, int cap = 3) : tFinished(false) {
    Pool(Actor *actor, int cap = 3) : tFinished(false) {
        counter = 0;
        for (int i = 0; i < cap; ++i) {
            workers.push_back(actor);
        }
//                    [this] {
//                        while (true) {
//                            std::function<void()> task;
//
//                            {
//                                std::unique_lock<std::mutex> lock(mx);
//                                cv.wait(lock,
//                                        [this] { return tFinished || !tasks.empty(); }
//                                );
//                                if (tFinished && tasks.empty())
//                                    return;
//
//                                // todo: compilation breaks here b/c Message is incompatible with function<void()>
//                                task = tasks.front();
//                                tasks.pop();
//                            }
//
//                            task();
//                        }
//                    }
//            );
//        }
    }

    ~Pool() {
        {
            unique_lock<mutex> lck(mx);
            tFinished = true;
        }

        cv.notify_all();
        for (Actor* a : workers)
            delete a;
    }

    // todo: this is equivalent of ThreadPool::enqueue
    // todo: needs love!!!
    void receive(Message* msg) {
        {
            unique_lock<mutex> lck(mx);
            tasks.push(msg);
        }

        cv.notify_one();
    }

};


#endif //__POOL__
