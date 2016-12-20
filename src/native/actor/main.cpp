#include "actor.hpp"
#include "pingpong.hpp"
#include "pool.hpp"

// todo:
#include <unistd.h>

using namespace std;


int main() {
    Pool<Ping> pool(1);
    pool.receive(new StartMessage());

    return 0;
}
