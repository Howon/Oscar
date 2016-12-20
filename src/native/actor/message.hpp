#ifndef __MESSAGE__
#define __MESSAGE__

#include <string>
#include <tuple>
#include <vector>

#include "actor.hpp"

using namespace std;

class Actor;

class Message {
// todo: make private
public:
    string name;
    Actor* sender;

public:
    Message(const string& name = "", Actor* const sender = NULL) :
        name(name), sender(sender) {}

    virtual ~Message() {}

    void setSender(Actor* sender) {
        this->sender = sender;
    }
};

static Message EMPTY_MESSAGE = Message();

// Ping messages
class StartMessage : public Message {
public:
    StartMessage(Actor *sender) : Message("start", sender) { }
    tuple<int> get() { return make_tuple(0); }
};

class PongMessage : public Message {
public:
    PongMessage(Actor *sender) : Message("pong", sender) { }
    tuple<int> get() { return make_tuple(NULL); }
};


// Pong messages
class PingMessage : public Message {
public:
    PingMessage(Actor *sender) : Message("ping", sender) { }
    tuple<int> get() { return make_tuple(NULL); }
};

class StopMessage : public Message {
public:
    StopMessage(Actor *sender) : Message("stop", sender) { }
    tuple<int> get() { return make_tuple(NULL); }
};

#endif //__MESSAGE__
