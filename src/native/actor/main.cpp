#include "pingpong.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
    auto pong = new Pong();
    auto ping = new Ping(pong, 99);

    ping->receive(new StartMessage(NULL));

    delete pong;
    delete ping;

    return 0;
}
