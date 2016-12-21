#include <oscar/actor.hpp>
#include <oscar/immut.hpp>

static Monitor *monitor;

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

class m_startMsg : public Message {
private:

public:
m_startMsg (Actor *sender) : Message("startMsg", sender)
{

}
tuple<> get() {
return make_tuple();
}
};

class m_stopMsg : public Message {
private:

public:
m_stopMsg (Actor *sender) : Message("stopMsg", sender)
{

}
tuple<> get() {
return make_tuple();
}
};

class m_pingMsg : public Message {
private:

public:
m_pingMsg (Actor *sender) : Message("pingMsg", sender)
{

}
tuple<> get() {
return make_tuple();
}
};

class m_pongMsg : public Message {
private:

public:
m_pongMsg (Actor *sender) : Message("pongMsg", sender)
{

}
tuple<> get() {
return make_tuple();
}
};


class a_Printer : public Actor {
private:
std::queue<m_die *> dieQueue;
std::queue<m_print *> printQueue;




public:
a_Printer () : Actor()
{
monitor->receive(new SpawnMessage());
t = thread([=] { consume(); });
}
virtual ~a_Printer() {
Die();
}

void Die() {
this->tFinished = true;
t.join();

monitor->receive(new DeleteMessage());
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

class a_Pong : public Actor {
private:
std::queue<m_die *> dieQueue;
std::queue<m_stopMsg *> stopMsgQueue;
std::queue<m_pingMsg *> pingMsgQueue;




public:
a_Pong () : Actor()
{
monitor->receive(new SpawnMessage());
t = thread([=] { consume(); });
}
virtual ~a_Pong() {
Die();
}

void Die() {
this->tFinished = true;
t.join();

monitor->receive(new DeleteMessage());
}
virtual void receive(Message *msg) {
if (m_die *pm = dynamic_cast<m_die *>(msg)) {
unique_lock<mutex> lck(mx);
dieQueue.push(pm);
goto notify;
}
if (m_stopMsg *pm = dynamic_cast<m_stopMsg *>(msg)) {
unique_lock<mutex> lck(mx);
stopMsgQueue.push(pm);
goto notify;
}
if (m_pingMsg *pm = dynamic_cast<m_pingMsg *>(msg)) {
unique_lock<mutex> lck(mx);
pingMsgQueue.push(pm);
goto notify;
}
notify:
cv.notify_one();
}
void respond(m_pingMsg *theMsgThatWasReceived) {

Println(std::string("  pong"));
theMsgThatWasReceived->sender->receive(new m_pongMsg(this));

delete theMsgThatWasReceived;
}

void respond(m_stopMsg *theMsgThatWasReceived) {

Println(std::string("pong stopped"));

delete theMsgThatWasReceived;
Die();
}

void respond(m_die *theMsgThatWasReceived) {


delete theMsgThatWasReceived;
Die();
}
void consume() {
unique_lock<mutex> lck(mx);
while (!this->tFinished){
while (dieQueue.empty()&&stopMsgQueue.empty()&&pingMsgQueue.empty()){
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
if (!stopMsgQueue.empty()) {
auto msg = stopMsgQueue.front();
stopMsgQueue.pop();
respond(msg);
goto loop;
}
if (!pingMsgQueue.empty()) {
auto msg = pingMsgQueue.front();
pingMsgQueue.pop();
respond(msg);
goto loop;
}
loop: ;
}
}};

class a_Ping : public Actor {
private:
a_Pong *pong;
int maxTurns;
std::queue<m_die *> dieQueue;
std::queue<m_pongMsg *> pongMsgQueue;
std::queue<m_startMsg *> startMsgQueue;

int count=0;
void incrementAndPrint()
{
(count = (count + 1));
Println(std::string("ping"));
}



public:
a_Ping (a_Pong *pong,int maxTurns) : Actor()
{
this->pong = pong;
this->maxTurns = maxTurns;
monitor->receive(new SpawnMessage());
t = thread([=] { consume(); });
}
virtual ~a_Ping() {
Die();
}

void Die() {
this->tFinished = true;
t.join();

monitor->receive(new DeleteMessage());
}
virtual void receive(Message *msg) {
if (m_die *pm = dynamic_cast<m_die *>(msg)) {
unique_lock<mutex> lck(mx);
dieQueue.push(pm);
goto notify;
}
if (m_pongMsg *pm = dynamic_cast<m_pongMsg *>(msg)) {
unique_lock<mutex> lck(mx);
pongMsgQueue.push(pm);
goto notify;
}
if (m_startMsg *pm = dynamic_cast<m_startMsg *>(msg)) {
unique_lock<mutex> lck(mx);
startMsgQueue.push(pm);
goto notify;
}
notify:
cv.notify_one();
}
void respond(m_startMsg *theMsgThatWasReceived) {

incrementAndPrint();
pong->receive(new m_pingMsg(this));

delete theMsgThatWasReceived;
}

void respond(m_pongMsg *theMsgThatWasReceived) {

incrementAndPrint();
if (count > maxTurns)
{
Println(std::string("ping stopped"));
pong->receive(new m_stopMsg(this));

Die();
} else {
pong->receive(new m_pingMsg(this));

}

delete theMsgThatWasReceived;
}

void respond(m_die *theMsgThatWasReceived) {


delete theMsgThatWasReceived;
Die();
}
void consume() {
unique_lock<mutex> lck(mx);
while (!this->tFinished){
while (dieQueue.empty()&&pongMsgQueue.empty()&&startMsgQueue.empty()){
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
if (!pongMsgQueue.empty()) {
auto msg = pongMsgQueue.front();
pongMsgQueue.pop();
respond(msg);
goto loop;
}
if (!startMsgQueue.empty()) {
auto msg = startMsgQueue.front();
startMsgQueue.pop();
respond(msg);
goto loop;
}
loop: ;
}
}};


int main () {
monitor = new Monitor();
cout << monitor->is_exitable() << endl;

auto pong=new a_Pong();

auto ping=new a_Ping(pong, 3);

ping->receive(new m_startMsg(NULL));

while (!monitor->is_exitable()) { }

cout << monitor->is_exitable() << endl;

return 0;
}

