#ifndef __MESSAGE__
#define __MESSAGE__

#include <string>
#include <tuple>
#include <vector>

#include "actor.hpp"

using namespace std;

enum MessageType { None, Some };

class Actor;

template <typename ...T>
class Message {
// todo: make private
public:
    MessageType type;

    string name;
    // todo: need to be able to pass various types in one list
//    tuple<T> formals;
    Actor* sender;

public:
    Message(const string& name="", MessageType type=None, Actor* const sender=NULL)
            : name(name), type(type), sender(sender)
    { }

//    virtual tuple<T...> get() = 0;
//    virtual tuple<T...> get();

    bool operator==(const Message& rhs) const {
        return (this->type == rhs.type &&
                this->name == rhs.name);
    }
};


class EmptyMessage : public Message<string> {
    string s;

public:
    EmptyMessage() : s("")
    { }

//    tuple<string> get() {
//        return make_tuple(s);
//    }
};

static EmptyMessage EMPTY_MESSAGE = EmptyMessage();


class HelloMessage : public Message<int, string> {
    int i;
    string s;

public:
    HelloMessage(int x, string y) : Message("hello", Some) {
        x = i;
        s = y;
    }

//    tuple<int, string> get() { return make_tuple(i, s); }
};


class ByeMessage : public Message<string> {
    string s;

public:
    ByeMessage(string y) : Message::Message("bye", Some) {
        s = y;
    }

//    tuple<string> get() { return make_tuple(s); }
};


#endif //__MESSAGE__
