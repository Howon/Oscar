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


template <typename T>
class Pool {
    vector<thread> workers;
//    queue< function<void()> > tasks;
    queue<Message*> tasks;

    condition_variable cv;
    mutex mx;
    bool tFinished;

//    vector<Ping> pings;
//    vector<Pong> pongs;
//
//    void consume() {
//        unique_lock<mutex> lck(mx);
//
//        while (true) {
//            while (q.empty())
//                cv.wait(lck);
//
//            int i = (count++) % pings.size();
//            Message* msg = q.front();
//            q.pop();
//
//            pings[i].receive(msg);
//        }
//    }

public:
    Pool(int cap = 3) : tFinished(false) {
        for (int i = 0; i < cap; ++i) {
            workers.push_back(
                    [this] {
                        while (true) {
                            std::function<void()> task;

                            {
                                std::unique_lock<std::mutex> lock(mx);
                                cv.wait(lock,
                                        [this] { return tFinished || !tasks.empty(); }
                                );
                                if (tFinished && tasks.empty())
                                    return;

                                // todo: compilation breaks here b/c Message is incompatible with function<void()>
                                task = tasks.front();
                                tasks.pop();
                            }

                            task();
                        }
                    }
            );
        }
    }

    ~Pool() {
        {
            unique_lock<mutex> lck(mx);
            tFinished = true;
        }

        cv.notify_all();
        for (thread& t : workers)
            t.join();
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
