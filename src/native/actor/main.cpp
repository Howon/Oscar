#include "actor.hpp"
#include "pingpong.hpp"
#include "pool.hpp"

// todo:
#include <unistd.h>

using namespace std;


int main() {
//    Ping ping;
//    Pong pong;
//
//    ping.setPong(&pong);
//
//    StartMessage startMessage;
//    ping.receive(&startMessage);
//
//    return 0;

    Pool pool(1);
    pool.receive(new StartMessage());

    return 0;
}
