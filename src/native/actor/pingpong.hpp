#ifndef __PINGPONG__
#define __PINGPONG__

#include "actor.hpp"

class Pong;

class Ping : public Actor {
    Actor* pong;
    queue<StartMessage *> startQueue;
    queue<PongMessage *> pongQueue;

    int currCount;
    int maxTurns;

    void die() { this->tFinished = true; }

    void consume() {
        unique_lock<mutex> lck(mx);

        while (!this->tFinished) {
            while (startQueue.empty() && pongQueue.empty()) {
                // finished running, terminate thread
                if (tFinished)
                    return;

                // else, wait for messages to arrive
                cv.wait(lck);
            }

            // process current message
            if (!startQueue.empty()) {
                auto msg = startQueue.front();
                startQueue.pop();
                respond(msg);
            } else if (!pongQueue.empty()) {
                auto msg = pongQueue.front();
                pongQueue.pop();
                respond(msg);
            }
        }
    }

    void incrementAndPrint() {
        ++currCount;
        printf("ping\n");
    }

    void respond(StartMessage *msg) {
        incrementAndPrint();
        pong->receive(new PingMessage(this));
    }

    void respond(PongMessage *msg) {
        incrementAndPrint();

        if (currCount > maxTurns) {
            printf("ping stopped\n");
            msg->sender->receive(new StopMessage(this));
            die();
        }

        msg->sender->receive(new PingMessage(this));
    }

public:
    void setPong(Actor* pong) {
        this->pong = pong;
    }

    Ping(int maxTurns=99) : currCount(0), maxTurns(maxTurns) {
        t = thread([=] { consume(); });
    }

    ~Ping() {
        t.join();
    }

    void receive(Message* const msg) {
        if (StartMessage *pm = dynamic_cast<StartMessage*>(msg)) {
            unique_lock<mutex> lck(mx);
            startQueue.push(pm);
        } else if (PongMessage* pm = dynamic_cast<PongMessage *>(msg)) {
            unique_lock<mutex> lck(mx);
            pongQueue.push(pm);
        }

        cv.notify_one();
    }
};


class Pong : public Actor {
    queue<StopMessage *> stopQueue;
    queue<PingMessage *> pingQueue;

    void die() { this->tFinished = true; }

    void consume() {
        unique_lock<mutex> lck(mx);

        while (!this->tFinished) {
            while (stopQueue.empty() && pingQueue.empty()) {
                // finished running, terminate thread
                if (tFinished)
                    return;

                // else, wait for messages to arrive
                cv.wait(lck);
            }

            // process current message
            if (!stopQueue.empty()) {
                auto msg = stopQueue.front();
                stopQueue.pop();
                respond(msg);
            } else if (!pingQueue.empty()) {
                auto msg = pingQueue.front();
                pingQueue.pop();
                respond(msg);
            }
        }
    }

    void respond(StopMessage *msg) {
        printf("pong stopped\n\n");
        die();
    }

    void respond(PingMessage *msg) {
        printf("  pong\n");
        msg->sender->receive(new PongMessage(this));
    }

public:
    Pong() {
        t = thread([=] { consume(); });
    }

    ~Pong() {
        t.join();
    }

    void receive(Message* const msg) {
        if (PingMessage* pm = dynamic_cast<PingMessage *>(msg)) {
            unique_lock<mutex> lck(mx);
            pingQueue.push(pm);
        }
        else if (StopMessage *pm = dynamic_cast<StopMessage*>(msg)) {
            unique_lock<mutex> lck(mx);
            stopQueue.push(pm);
        }

        cv.notify_one();
    }
};


#endif //__PINGPONG__
