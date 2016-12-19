#include "actor.hpp"
using namespace std;

// todo: keep a global map of <address, Actor> for communication


int main() {
    Actor a;

    HelloMessage hm(1, "hello");
    ByeMessage bm("bye");

    a.receive(hm);
    a.receive(bm);

    return 0;
}