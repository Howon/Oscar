#ifndef __PINGPONG__
#define __PINGPONG__

#include "actor.hpp"

class Pong;

class Ping : public Actor {
    Actor* pong;
    queue<StartMessage> startQueue;
    queue<PongMessage> pongQueue;

    int currCount;
    int maxTurns;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
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

                // send a response to sender
                if (auto response = respond(msg)) {
                    pong->receive(response);
                    delete response;
                }
            } else if (!pongQueue.empty()) {
                auto msg = pongQueue.front();
                pongQueue.pop();

                // send a response to sender
                if (auto response = respond(msg)) {
                    pong->receive(response);
                    delete response;
                }
            }
        }
    }

    void incrementAndPrint() {
        ++currCount;
        printf("ping\n");
    }

    Message* respond(StartMessage msg) {
        incrementAndPrint();
        return new PingMessage();
    }

    Message* respond(PongMessage msg) {
        incrementAndPrint();

        if (currCount > maxTurns) {
            printf("ping stopped\n");
            tFinished = true;
            return new StopMessage();
        }

        return new PingMessage();
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
            startQueue.push(*pm);
        } else if (PongMessage* pm = dynamic_cast<PongMessage *>(msg)) {
            unique_lock<mutex> lck(mx);
            pongQueue.push(*pm);
        }

        cv.notify_one();
    }
};


class Pong : public Actor {
    Actor* ping;
    queue<StopMessage> stopQueue;
    queue<PingMessage> pingQueue;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
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

                // send a response to sender
                if (auto response = respond(msg)) {
                    ping->receive(response);
                    delete response;
                }
            } else if (!pingQueue.empty()) {
                auto msg = pingQueue.front();
                pingQueue.pop();

                // send a response to sender
                if (auto response = respond(msg)) {
                    ping->receive(response);
                    delete response;
                }
            }
        }
    }

    Message* respond(StopMessage msg) {
        printf("pong stopped\n\n");
        tFinished = true;
        return NULL;
    }

    Message* respond(PingMessage msg) {
        printf("  pong\n");
        return new PongMessage();
    }

public:
    void setPing(Actor* ping) {
        this->ping = ping;
    }

    Pong() {
        t = thread([=] { consume(); });
    }

    ~Pong() {
        t.join();
    }

    void receive(Message* const msg) {
        if (PingMessage* pm = dynamic_cast<PingMessage *>(msg)) {
            unique_lock<mutex> lck(mx);
            pingQueue.push(*pm);
        }
        else if (StopMessage *pm = dynamic_cast<StopMessage*>(msg)) {
            unique_lock<mutex> lck(mx);
            stopQueue.push(*pm);
        }

        cv.notify_one();
    }
};


#endif //__PINGPONG__
