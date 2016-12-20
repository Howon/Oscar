#include "actor.hpp"
#include "pingpong.hpp"
#include "pool.hpp"
using namespace std;


int main() {
    Pool pool(new Ping(1, new Pong()), 3);
    pool.receive(new StartMessage());

    return 0;
}
