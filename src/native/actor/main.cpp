#include "pingpong.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
    Ping ping;
    Pong pong;

    ping.setPong(&pong);

    ping.receive(new StartMessage(NULL));

    return 0;
}
