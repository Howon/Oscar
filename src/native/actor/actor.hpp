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
    queue<HelloMessage> helloQueue;
    queue<ByeMessage> byeQueue;

    std::thread t;
    condition_variable cv;
    mutex mx;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (true) {
            while (messageQueue.empty())
                cv.wait(lck);

            // process current message
            const string &msgName = messageQueue.front();
            messageQueue.pop();

            if (msgName == "hello") {
                HelloMessage msg = helloQueue.front();
                helloQueue.pop();

                // send a response to sender
                HelloMessage response = respond(msg);
                if (msg.sender && Message(response) != EMPTY_MESSAGE)
                    msg.sender->receive(response);
            } else if (msgName == "bye") {
                auto msg = byeQueue.front();
                byeQueue.pop();

                // send a response to sender
                auto response = respond(msg);
                if (msg.sender && Message(response) != EMPTY_MESSAGE)
                    msg.sender->receive(response);
            }
        }
    }

    HelloMessage respond(HelloMessage msg) {
        cout << msg.name << endl;
        return msg;
    }

    ByeMessage respond(ByeMessage msg) {
        cout << msg.name << endl;
        return msg;
//        return ByeMessage(EMPTY_MESSAGE);
    }

public:
    Actor() {
        t = thread([=] { consume(); });
    }

    ~Actor() {
        t.join();
    }

    void receive(const HelloMessage &msg) {
        {
            unique_lock<mutex> lck(mx);
            messageQueue.push(msg.name);
            helloQueue.push(msg);
        }

        //if (q.size() == 1)
        cv.notify_one();
    }

    void receive(const ByeMessage &msg) {
        {
            unique_lock<mutex> lck(mx);
            messageQueue.push(msg.name);
            byeQueue.push(msg);
        }

        //if (q.size() == 1)
        cv.notify_one();
    }


    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};


#endif  // __ACTOR__