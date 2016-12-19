#include "actor.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
    Ping ping;
    Pong pong;

    ping.setPong(&pong);
    pong.setPing(&ping);

    StartMessage startMessage;
    ping.receive(&startMessage);

    return 0;
}