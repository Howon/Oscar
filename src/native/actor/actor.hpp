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

    // synchro shit
    std::thread t;
    condition_variable cv;
    mutex mx;

    void consume() {
        unique_lock<mutex> lck(mx);

        while (messageQueue.empty())
            cv.wait(lck);

        while (!messageQueue.empty()) {
            // process current message
            const string& msgName = messageQueue.front();
            messageQueue.pop();

            if (msgName == "hello") {
                if (helloQueue.empty())
                    continue;

                HelloMessage msg = helloQueue.front();
                helloQueue.pop();

                // send a response to sender
                HelloMessage response = respond(msg);
//                if (msg.sender && response != EMPTY_MESSAGE)
                if (msg.sender)
                    msg.sender->receive(response);
            } else if (msgName == "bye") {
                if (byeQueue.empty())
                    continue;

                auto msg = byeQueue.front();
                byeQueue.pop();

                // send a response to sender
                auto response = respond(msg);
//                if (msg.sender && response != EMPTY_MESSAGE)
                if (msg.sender)
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
//        return EMPTY_MESSAGE;
        return msg;
    }

public:
    Actor() {
        t = thread([=] { consume(); });
    }

    ~Actor() {
        t.join();
    }

    void receive(const HelloMessage& msg) {
        unique_lock<mutex> lck(mx);

        messageQueue.push(msg.name);
        helloQueue.push(msg);
        //if (q.size() == 1)
        cv.notify_one();
    }

    void receive(const ByeMessage& msg) {
        unique_lock<mutex> lck(mx);

        messageQueue.push(msg.name);
        byeQueue.push(msg);
        //if (q.size() == 1)
        cv.notify_one();
    }


    // todo: implement
//    void send(int actorAddr, const Message& msg) { }
//    void broadcast(int poolAddr, const Message& msg) { }
};

//using umST = unordered_map<string, fxnPtr<T>>;

#endif  // __ACTOR__