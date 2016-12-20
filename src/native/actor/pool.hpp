#ifndef __POOL__
#define __POOL__

#include <vector>

#include "actor.hpp"


template <typename T>
class Pool {
// todo: make something private
public:
    // circular buffer
    vector<T> cbuff;
//    vector<Actor*> cbuff;
    int count;

    Pool(int size=5) : count(0) {
        cbuff.resize(size, T());
    }

    Pool(const vector<Actor*>& actors) : count(0) {
        cbuff = actors;
    }

    void receive(const vector<Message*>& msgs) {
        for (Message* msg : msgs) {
            int i = (count++) % cbuff.size();
            cbuff[i]->receive(msg);
        }
    }
};


#endif //__POOL__


/*

I know how to do pool
- we need a round robing group of actors
- that we can pass messages to
- pool<Actor> right
-- we can implement it as a vector of pools

and whnever we received call the next actor element‘s send method
 * */