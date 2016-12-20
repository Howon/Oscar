#include "actor.hpp"
#include "pingpong.hpp"
#include "pool.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
//    Ping ping;
//    Pong pong;
//
//    ping.setPong(&pong);
//
//    StartMessage startMessage(&ping);
//    ping.receive(&startMessage);

//    Pool<Ping> pings(2);
//    Pool<Pong> pongs(2);
//
//    for (int i = 0; i < pings.size(); ++i)
//        pings[i].setPong(&pongs[i]);

//    int numActors = 2;
//    vector<Actor*> pings(numActors, new Ping(2));
//    vector<Actor*> pongs(numActors, new Pong());
//
//    vector<Message*> startMessages(numActors, new StartMessage(NULL));
//
//    // assign things
//    for (int i = 0; i < pings.size(); ++i) {
//        ((Ping*)pings[i])->setPong(pongs[i]);
//        ((StartMessage*)startMessages[i])->setSender(pings[i]);
//    }
// start mayhem
//    for (int i = 0; i < pings.size(); ++i)
//        pings[i]->receive(startMessages[i]);

    int numActors = 2;
    vector<Ping> pings(numActors);
    vector<Pong> pongs(numActors);

    vector<StartMessage> startMessages(numActors, NULL);

    // assign things
    for (int i = 0; i < pings.size(); ++i) {
        pings[i].setPong(&pongs[i]);
        startMessages[i].setSender(&pings[i]);
    }

    // start mayhem
    for (int i = 0; i < pings.size(); ++i)
        pings[i].receive(&startMessages[i]);

//    Pool<Actor*> pool(pings);
//    pool.receive(startMessages);

    return 0;
}
