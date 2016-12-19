#include "actor.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
//    Actor a;
//
//    Actor b;
//
//    HelloMessage hm(1, "hello");
//    ByeMessage bm("bye");
//
//    a.receive(hm);
//    sleep(2);
//    a.receive(bm);
//
//    sleep(2);
//
//    a.receive(hm);
//    sleep(2);
//    a.receive(bm);


    Ping ping;
    Pong pong;

    ping.setPong(&pong);
    pong.setPing(&ping);

    StartMessage startMessage;
    ping.receive(&startMessage);


//    PongMessage pongMessage;
//    pongMessage.setSender(&pong);

//    PingMessage pingMessage;
//    pingMessage.setSender(&ping);

//    StopMessage stopMessage;
//    stopMessage.setSender(&ping);

    return 0;
}