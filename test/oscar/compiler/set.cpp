#include <oscar/actor.hpp>
#include <oscar/immut.hpp>
static Monitor *___monitor;

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

class m_payload : public Message {
private:
std::string msg;

public:
m_payload (std::string msg,Actor *sender) : Message("payload", sender)
{
this->msg = msg;

}
tuple<std::string> get() {
return make_tuple(this->msg);
}
};

class m_getLog : public Message {
private:

public:
m_getLog (Actor *sender) : Message("getLog", sender)
{

}
tuple<> get() {
return make_tuple();
}
};

class m_logPayLoad : public Message {
private:
immut::list<std::string> mlog;

public:
m_logPayLoad (immut::list<std::string> mlog,Actor *sender) : Message("logPayLoad", sender)
{
this->mlog = mlog;

}
tuple<immut::list<std::string>> get() {
return make_tuple(this->mlog);
}
};


class a_Printer : public Actor {
private:
std::queue<m_die *> dieQueue;
std::queue<m_print *> printQueue;




public:
a_Printer (): Actor()
{

___monitor->receive(new SpawnMessage());
t = thread([=] { consume(); });
}
virtual ~a_Printer() { t.join(); }

void Die() {
this->tFinished = true;
___monitor->receive(new DeleteMessage());
cv.notify_one();
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
void respond(m_print *___msgRcvd) {
auto msg = get<0>(___msgRcvd->get());

Println(msg);
delete ___msgRcvd;
}

void respond(m_die *___msgRcvd) {


delete ___msgRcvd;
Die();
}
void consume() {
unique_lock<mutex> lck(mx);
while (!this->tFinished) {
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

class a_Echo : public Actor {
private:
std::queue<m_die *> dieQueue;
std::queue<m_payload *> payloadQueue;




public:
a_Echo (): Actor()
{

___monitor->receive(new SpawnMessage());
t = thread([=] { consume(); });
}
virtual ~a_Echo() { t.join(); }

void Die() {
this->tFinished = true;
___monitor->receive(new DeleteMessage());
cv.notify_one();
}
virtual void receive(Message *msg) {
if (m_die *pm = dynamic_cast<m_die *>(msg)) {
unique_lock<mutex> lck(mx);
dieQueue.push(pm);
goto notify;
}
if (m_payload *pm = dynamic_cast<m_payload *>(msg)) {
unique_lock<mutex> lck(mx);
payloadQueue.push(pm);
goto notify;
}
notify:
cv.notify_one();
}
void respond(m_payload *___msgRcvd) {
auto msg = get<0>(___msgRcvd->get());

___msgRcvd->sender->receive(new m_payload((std::string("Hello ") + msg), this));

delete ___msgRcvd;
}

void respond(m_die *___msgRcvd) {


delete ___msgRcvd;
Die();
}
void consume() {
unique_lock<mutex> lck(mx);
while (!this->tFinished) {
while (dieQueue.empty()&&payloadQueue.empty()){
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
if (!payloadQueue.empty()) {
auto msg = payloadQueue.front();
payloadQueue.pop();
respond(msg);
goto loop;
}
loop: ;
}
}};

class a_Logger : public Actor {
private:
std::queue<m_die *> dieQueue;
std::queue<m_getLog *> getLogQueue;
std::queue<m_payload *> payloadQueue;

immut::list<std::string> log=immut::list<string>{};


public:
a_Logger (): Actor()
{

___monitor->receive(new SpawnMessage());
t = thread([=] { consume(); });
}
virtual ~a_Logger() { t.join(); }

void Die() {
this->tFinished = true;
___monitor->receive(new DeleteMessage());
cv.notify_one();
}
virtual void receive(Message *msg) {
if (m_die *pm = dynamic_cast<m_die *>(msg)) {
unique_lock<mutex> lck(mx);
dieQueue.push(pm);
goto notify;
}
if (m_getLog *pm = dynamic_cast<m_getLog *>(msg)) {
unique_lock<mutex> lck(mx);
getLogQueue.push(pm);
goto notify;
}
if (m_payload *pm = dynamic_cast<m_payload *>(msg)) {
unique_lock<mutex> lck(mx);
payloadQueue.push(pm);
goto notify;
}
notify:
cv.notify_one();
}
void respond(m_payload *___msgRcvd) {
auto msg = get<0>(___msgRcvd->get());

Println(msg);
Append(msg, log);
Println(Append(msg, log));
delete ___msgRcvd;
}

void respond(m_getLog *___msgRcvd) {


delete ___msgRcvd;
}

void respond(m_die *___msgRcvd) {


delete ___msgRcvd;
Die();
}
void consume() {
unique_lock<mutex> lck(mx);
while (!this->tFinished) {
while (dieQueue.empty()&&getLogQueue.empty()&&payloadQueue.empty()){
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
if (!getLogQueue.empty()) {
auto msg = getLogQueue.front();
getLogQueue.pop();
respond(msg);
goto loop;
}
if (!payloadQueue.empty()) {
auto msg = payloadQueue.front();
payloadQueue.pop();
respond(msg);
goto loop;
}
loop: ;
}
}};


int main () {
___monitor = new Monitor();

Println(immut::set<int>{});
Println(immut::set<int>{1});
Println(immut::set<int>{2, 2, 2});
Println((immut::set<int>{1, 2, 3} == immut::set<int>{3, 2, 1}));

usleep(1000);
while (!___monitor->is_exitable()) {cout << "";}
delete ___monitor;
return 0;
}

