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
class SpawnMessage : public Message {
public:
    SpawnMessage() : Message("spawn") { }
};

// Ping messages
class DeleteMessage : public Message {
public:
    DeleteMessage() : Message("delete") { }
};

#endif //__MESSAGE__
