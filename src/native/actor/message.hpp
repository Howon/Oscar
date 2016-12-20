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
};

// Ping messages
class StartMessage : public Message {
public:
    StartMessage(Actor *sender) : Message("start", sender) { }
    tuple<> get() { return make_tuple(); }
};

class PongMessage : public Message {
public:
    PongMessage(Actor *sender) : Message("pong", sender) { }
    tuple<> get() { return make_tuple(); }
};


// Pong messages
class PingMessage : public Message {
public:
    PingMessage(Actor *sender) : Message("ping", sender) { }
    tuple<> get() { return make_tuple(); }
};

class StopMessage : public Message {
public:
    StopMessage(Actor *sender) : Message("stop", sender) { }
    tuple<> get() { return make_tuple(); }
};

#endif //__MESSAGE__
