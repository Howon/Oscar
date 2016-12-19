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
    // todo: hacky shit
    queue<string> messageQueue;
//    queue<HelloMessage> helloQueue;
//    queue<ByeMessage> byeQueue;

    std::thread t;
    condition_variable cv;
    mutex mx;

//    void consume() {
//        unique_lock<mutex> lck(mx);
//
//        while (true) {
//            while (messageQueue.empty())
//                cv.wait(lck);
//
//            // process current message
//            const string &msgName = messageQueue.front();
//            messageQueue.pop();
//
//            if (msgName == "hello") {
//                HelloMessage msg = helloQueue.front();
//                helloQueue.pop();
//
//                // send a response to sender
//                HelloMessage response = respond(msg);
//                if (msg.sender && Message(response) != EMPTY_MESSAGE)
//                    msg.sender->receive(response);
//            } else if (msgName == "bye") {
//                auto msg = byeQueue.front();
//                byeQueue.pop();
//
//                // send a response to sender
//                auto response = respond(msg);
//                if (msg.sender && Message(response) != EMPTY_MESSAGE)
//                    msg.sender->receive(response);
//            }
//        }
//    }
//
//
//    HelloMessage respond(HelloMessage msg) {
//        cout << msg.name << endl;
//        return msg;
//    }
//
//    ByeMessage respond(ByeMessage msg) {
//        cout << msg.name << endl;
//        return msg;
////        return ByeMessage(EMPTY_MESSAGE);
//    }

public:
    Actor() {
//        t = thread([=] { consume(); });
    }

    ~Actor() {
//        t.join();
    }


    virtual void receive(Message* msg) = 0;
//    virtual void receive(StartMessage msg) = 0;
//    virtual void receive(StartMessage msg) = 0;
//    virtual void receive(PongMessage msg) = 0;
//    virtual void receive(StopMessage msg) = 0;
//    virtual void receive(PingMessage msg) = 0;

//    void receive(const HelloMessage &msg) {
//        {
//            unique_lock<mutex> lck(mx);
//            messageQueue.push(msg.name);
//            helloQueue.push(msg);
//        }
//
//        //if (q.size() == 1)
//        cv.notify_one();
//    }
//
//    void receive(const ByeMessage &msg) {
//        {
//            unique_lock<mutex> lck(mx);
//            messageQueue.push(msg.name);
//            byeQueue.push(msg);
//        }
//
//        //if (q.size() == 1)
//        cv.notify_one();
//    }


    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};

class Pong;

class Ping : public Actor {
public:

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
            while (messageQueue.empty())
                cv.wait(lck);

            // process current message
            const string &msgName = messageQueue.front();
            messageQueue.pop();

            if (msgName == "start") {
                auto msg = startQueue.front();
                startQueue.pop();

                // send a response to sender
//                auto response = respond(msg);
                Message response = respond(msg);
                if (response != EMPTY_MESSAGE)
                    msg.sender->receive(&response);
            } else if (msgName == "pong") {
                auto msg = pongQueue.front();
                pongQueue.pop();

                // send a response to sender
                auto response = respond(msg);
                if (response != EMPTY_MESSAGE)
                    msg.sender->receive(&response);
            }
        }
    }

    void incrementAndPrint() {
        ++count;
        printf("ping\n");
    }

    Message respond(StartMessage msg) {
        incrementAndPrint();

        PingMessage pingMessage;
        pingMessage.setSender(this);
        return pingMessage;
//        return PingMessage();
    }

    Message respond(PongMessage msg) {
        incrementAndPrint();

        if (count > 99) {
            printf("ping stopped\n");
//            context.stop(self)

            StopMessage stopMessage;
            stopMessage.setSender(this);
            return stopMessage;
//            return StopMessage();
        }

        PingMessage pingMessage;
        pingMessage.setSender(this);
        return pingMessage;
//        return PingMessage();
    }


    void receive(Message* msg) {
        if (StartMessage *pm = dynamic_cast<StartMessage*>(msg)) {
            {
                unique_lock<mutex> lck(mx);
                messageQueue.push(pm->name);
                startQueue.push(*pm);
            }
        } else if (PongMessage* pm = dynamic_cast<PongMessage *>(msg)) {
            {
                unique_lock<mutex> lck(mx);
                messageQueue.push(msg->name);
                pongQueue.push(*pm);
            }
        }

        //if (q.size() == 1)
        cv.notify_one();
    }
};


class Pong : public Actor {
public:
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
            while (messageQueue.empty())
                cv.wait(lck);

            // process current message
            const string &msgName = messageQueue.front();
            messageQueue.pop();

            if (msgName == "stop") {
                auto msg = stopQueue.front();
                stopQueue.pop();

                // send a response to sender
                auto response = respond(msg);
                if (response != EMPTY_MESSAGE)
                    msg.sender->receive(&response);
            } else if (msgName == "pong") {
                auto msg = pingQueue.front();
                pingQueue.pop();

                // send a response to sender
                auto response = respond(msg);
                if (response != EMPTY_MESSAGE)
                    msg.sender->receive(&response);
            }
        }
    }

    Message respond(StopMessage msg) {
        printf("pong stopped\n\n");
        return EMPTY_MESSAGE;
    }

    Message respond(PingMessage msg) {
        printf("  pong\n");

        PongMessage pongMessage;
        pongMessage.setSender(this);
        return pongMessage;
//        return PongMessage();
    }

    void receive(Message* msg) {
        if (StopMessage *pm = dynamic_cast<StopMessage*>(msg)) {
            {
                unique_lock<mutex> lck(mx);
                messageQueue.push(pm->name);
                stopQueue.push(*pm);
            }
        } else if (PingMessage* pm = dynamic_cast<PingMessage *>(msg)) {
            {
                unique_lock<mutex> lck(mx);
                messageQueue.push(msg->name);
                pingQueue.push(*pm);
            }
        }

        //if (q.size() == 1)
        cv.notify_one();
    }
};

#endif  // __ACTOR__