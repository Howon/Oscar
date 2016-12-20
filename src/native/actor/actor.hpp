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

    void Die() { this->tFinished = true; }
};

class Monitor : public Actor {
    bool end;
    atomic<int> actor_counter;

    queue<SpawnMessage *> spawnQueue;
    queue<DeleteMessage *> deleteQueue;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (!this->tFinished) {
            while (spawnQueue.empty() && deleteQueue.empty()) {
                if (tFinished)
                    return;
                cv.wait(lck);
            }

            if (!spawnQueue.empty()) {
                auto msg = spawnQueue.front();
                spawnQueue.pop();
                respond(msg);
            } else if (!deleteQueue.empty()) {
                auto msg = deleteQueue.front();
                deleteQueue.pop();
                respond(msg);
            }
        }
    }

    void respond(SpawnMessage *msg) {
        actor_counter++;
        end = false;

        delete msg;
    }

    void respond(DeleteMessage *msg) {
        actor_counter--;

        delete msg;

        if (actor_counter.load() == 0 && spawnQueue.empty() &&
            deleteQueue.empty())
            this->end = true;
    }

public:
    Monitor() {
        this->end = true;
        actor_counter = 0;
        t = thread([=] { consume(); });
    }

    virtual ~Monitor() {
        this->end = true;

        t.join();
    }

    bool is_exitable() { return this->end; }

    void receive(Message* const msg) {
        if (SpawnMessage* pm = dynamic_cast<SpawnMessage *>(msg)) {
            unique_lock<mutex> lck(mx);
            spawnQueue.push(pm);
        } else if (DeleteMessage *pm = dynamic_cast<DeleteMessage *>(msg)) {
            unique_lock<mutex> lck(mx);
            deleteQueue.push(pm);
        }

        cv.notify_one();
    }
};

#endif  // __ACTOR__
