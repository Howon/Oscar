#include <oscar/actor.hpp>
#include <oscar/immut.hpp>
class m_die : public Message {
private:

public:
m_die (Actor *sender) : Message("die", sender)
{

}
tuple<> get() {
return make_tuple();
}
};

class m_print : public Message {
private:
std::string msg;

public:
m_print (std::string msg,Actor *sender) : Message("print", sender)
{
this->msg = msg;

}
tuple<std::string> get() {
return make_tuple(this->msg);
}
};


class a_Printer : public Actor {
private:
std::queue<m_die *> dieQueue;
std::queue<m_print *> printQueue;




public:
a_Printer () : Actor()
{

t = thread([=] { consume(); });
}
virtual ~a_Printer() {
Die();
}

void Die() {
this->tFinished = true;
t.join();
}
virtual void receive(Message *msg) {
if (m_die *pm = dynamic_cast<m_die *>(msg)) {
unique_lock<mutex> lck(mx);
dieQueue.push(pm);
goto notify;
}
if (m_print *pm = dynamic_cast<m_print *>(msg)) {
unique_lock<mutex> lck(mx);
printQueue.push(pm);
goto notify;
}
notify:
cv.notify_one();
}
void respond(m_print *theMsgThatWasReceived) {
auto msg = get<0>(theMsgThatWasReceived->get());

Println(msg);
delete theMsgThatWasReceived;
}

void respond(m_die *theMsgThatWasReceived) {


delete theMsgThatWasReceived;
Die();
}
void consume() {
unique_lock<mutex> lck(mx);
while (!this->tFinished){
while (dieQueue.empty()&&printQueue.empty()){
if (tFinished)
return;
cv.wait(lck);
}
if (!dieQueue.empty()) {
auto msg = dieQueue.front();
dieQueue.pop();
respond(msg);
goto loop;
}
if (!printQueue.empty()) {
auto msg = printQueue.front();
printQueue.pop();
respond(msg);
goto loop;
}
loop: ;
}
}};


int main () {
Println(true);
Println(false);
Println(true);
Println(false);
Println(true);
Println(false);
Println(true);
Println(false);
Println(true);
Println(false);
Println(true);
Println(false);
return 0;
}

