#include "pingpong.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
    Ping ping;
    Pong pong;

    ping.setPong(&pong);

    StartMessage startMessage(&ping);
    ping.receive(&startMessage);

    return 0;
}
