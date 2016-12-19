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

public:
    Actor() { }

    ~Actor() { }

    virtual void receive(Message* msg) = 0;

    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};

class Pong;

class Ping : public Actor {
public:
    // Pong ref
    Actor* pong;
    void setPong(Actor* pong) {
        this->pong = pong;
    }

    int count;
    Ping() : count(0)
    {
        t = thread([=] { consume(); });
    }

    ~Ping() {
        t.join();
    }

    queue<StartMessage> startQueue;
    queue<PongMessage> pongQueue;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (startQueue.empty() && pongQueue.empty())
                cv.wait(lck);

            // process current message
            if (!startQueue.empty()) {
                auto msg = startQueue.front();  startQueue.pop();

                // send a response to sender
                if (auto response = respond(msg))
                    pong->receive(response);
            } else if (!pongQueue.empty()) {
                auto msg = pongQueue.front();
                pongQueue.pop();

                // send a response to sender
                if (auto response = respond(msg))
                    pong->receive(response);
            }
        }
    }

    void incrementAndPrint() {
        ++count;
        printf("ping\n");
    }

    Message* respond(StartMessage msg) {
        incrementAndPrint();
        return new PingMessage();
    }

    Message* respond(PongMessage msg) {
        incrementAndPrint();

        if (count > 99) {
            printf("ping stopped\n");
            // todo: stop itself
//            context.stop(self)

            return new StopMessage();
        }

        // todo: don't leak
        return new PingMessage();
    }


    void receive(Message* msg) {
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
public:
    // Pong ref
    Actor* ping;
    void setPing(Actor* ping) {
        this->ping = ping;
    }

    Pong()
    {
        t = thread([=] { consume(); });
    }

    ~Pong() {
        t.join();
    }

    queue<StopMessage> stopQueue;
    queue<PingMessage> pingQueue;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (stopQueue.empty() && pingQueue.empty())
                cv.wait(lck);

            // process current message
            if (!stopQueue.empty()) {
                auto msg = stopQueue.front();
                stopQueue.pop();

                // send a response to sender
                if (auto response = respond(msg))
                    ping->receive(response);
            } else if (!pingQueue.empty()) {
                auto msg = pingQueue.front();
                pingQueue.pop();

                // send a response to sender
                if (auto response = respond(msg))
                    ping->receive(response);
            }
        }
    }

    Message* respond(StopMessage msg) {
        printf("pong stopped\n\n");
        // todo: stop itself
        return NULL;
    }

    Message* respond(PingMessage msg) {
        printf("  pong\n");
        return new PongMessage();
    }

    void receive(Message* msg) {
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

#endif  // __ACTOR__