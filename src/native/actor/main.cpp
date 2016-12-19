#include "actor.hpp"
using namespace std;

#include <unistd.h>

// todo: keep a global map of <address, Actor> for communication


int main() {
    Actor a;

    Actor b;

    HelloMessage hm(1, "hello");
    ByeMessage bm("bye");

    a.receive(hm);
    sleep(2);
    a.receive(bm);

    sleep(2);

    a.receive(hm);
    sleep(2);
    a.receive(bm);

    return 0;
}