#include "actor.hpp"
#include "pingpong.hpp"
#include "pool.hpp"

// todo:
#include <unistd.h>

using namespace std;


int main() {
//    Pool<Ping, int, string> pool(1);

//    Pong pong;
    Pool pool(new Ping(2, new Pong()), 1);
    pool.receive(new StartMessage());

    return 0;
}
