#ifndef __PINGPONG__
#define __PINGPONG__

#include "actor.hpp"
#include "../immut/immut.hpp"


class Pong : public Actor {
    queue<StopMessage *> stopQueue;
    queue<PingMessage *> pingQueue;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (!this->tFinished) {
            while (stopQueue.empty() && pingQueue.empty()) {
                // finished running, terminate thread
                if (tFinished)
                    return;

                // else, wait for messages to arrive
                cv.wait(lck);
                goto loop;
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
                goto loop;
            }
            loop: ;
        }
    }

    void respond(StopMessage *msg) {
        delete msg;
        printf("pong stopped\n\n");
        Die();
    }

    void respond(PingMessage *msg) {
        printf("  pong\n");
        msg->sender->receive(new PongMessage(this));

        delete msg;
    }

public:
    Pong() {
        t = thread([=] { consume(); });
    }

    virtual ~Pong() {
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

class Ping : public Actor {
    Pong* pong;

    queue<StartMessage *> startQueue;
    queue<PongMessage *> pongQueue;

    int count = 0;
    int maxTurns;

        void consume()
    {
        unique_lock<mutex> lck(mx);
        while (!this->tFinished) {
            while (pongQueue.empty() && startQueue.empty()) {
                if (tFinished)
                    return;
                cv.wait(lck);
            }
            if (!pongQueue.empty()) {
                auto msg = pongQueue.front();
                pongQueue.pop();
                respond(msg);
                goto loop;
            }
            if (!startQueue.empty()) {
                auto msg = startQueue.front();
                startQueue.pop();
                respond(msg);
                goto loop;
            }
        loop:;
        }
    }

    void incrementAndPrint() {
        ++count;
        printf("ping\n");
    }

    void respond(StartMessage *msg) {
        delete msg;
        incrementAndPrint();
        pong->receive(new PingMessage(this));
    }


    void respond(PongMessage* msg)
    {
        delete msg;
        incrementAndPrint();
        if (count > maxTurns) {
            Println(std::string("ping stopped"));
            pong->receive(new StopMessage(this));

            Die();
        }
        else {
            pong->receive(new PingMessage(this));
        }
    }

public:
    Ping(Pong* pong, int maxTurns=99) : Actor() {
        this->pong = pong;
        this->maxTurns = maxTurns;

        t = thread([=] { consume(); });
    }

    virtual ~Ping() {
        t.join();
    }

    void receive(Message* const msg) {
       if (PongMessage* pm = dynamic_cast<PongMessage*>(msg)) {
            unique_lock<mutex> lck(mx);
            pongQueue.push(pm);
            goto notify;
        }
        if (StartMessage* pm = dynamic_cast<StartMessage*>(msg)) {
            unique_lock<mutex> lck(mx);
            startQueue.push(pm);
            goto notify;
        }
    notify:
        cv.notify_one();
    }
};


#endif //__PINGPONG__
