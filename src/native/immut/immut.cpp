#include "collection.h"

#include <vector>
#include <string>

using namespace immut;

template <typename F>
void ForEach(const F f, OscarType *t)
{
    switch (t->t) {
        case CL : t->cl->forEach(f);
        case CS : t->cs->forEach(f);
        case CM : t->cm->forEach(f);
        default : {
          throw invalid_argument("ForEach can only be used with containers");
        }
    }
}

template <typename F>
OscarType *FoldLeft(F f, OscarType *acc, OscarType *lst)
{
    assert(lst->c == CL);

    ForEach([&f, &acc](const OscarType &v){
        acc = f(acc, v);
    }, lst);

    return acc;
}

OscarType *Reverse(OscarType *lst)
{
    assert(lst->c == CL);

    return FoldLeft([](const OscarType *acc, OscarType v) {
        return list(v, *acc->cl);
    }, new OscarType(), lst);
}

size_t Size(OscarType *c)
{
    assert(c->t == CL || c->t == CS || c->t == CM);

    size_t size = 0;

    if (c->t == CL || c->t == CS) {
        ForEach([&size](OscarType t) {
            size++;
        }, c);
    } else {
        ForEach([&size](OscarType k, OscarType v) {
            size++;
        }, c);
    }

    return size;
}

OscarType *Put(OscarType x, OscarType *s)
{
    assert(c->t == CS);

    OscarType *res = new OscarType();
    *res->cs = s->cs->inserted(x);

    return res;
}

OscarType Put(OscarType k, OscarType v, OscarType *m)
{
    assert(c->t == CM);

    OscarType *res = new OscarType();
    *res->cm = m->cm->inserted(k, v);

    return res;
}

template <typename F>
OscarType *Filter(F f, OscarType t)
{
    assert(c->t == CL || c->t == CS || c->t == CM);

    OscarType *res = new OscarType();

    if (t.t == CL) {
        ForEach([&f, &res, &t](OscarType const &v){
            if (f(v))
                *res->cl = res->cl->push_front(v);
        }, t);

        return Reverse(res);
    } else if (t.t == CS) {
        ForEach([&f, &res, &t](OscarType const &v){
            if (f(v))
                *res->cs = res->cs->inserted(v);
        }, t);
    } else {
        ForEach([&f, &res, &t](const OscarType &k, const OscarType &v){
            if (f(k, v))
                *res->cm = res->cm->inserted(k, v);
        }, t);
    }

    return res;
}

bool Contains(OscarType t, OscarType *c)
{
    assert(c->t == CL || c->t == CS || c->t == CM);

    if (c->t == CL)
        return c->cl->contains(t);
    if (c->t == CS)
        return c->cs->contains(t);

    return c->cm->contains(t);
}

bool Contains(OscarType t, set s)
{
    return s.contains(t);
}

bool Contains(OscarType k, map m)
{
    return m.contains(k);
}

template <typename F>
OscarType *Map(F f, OscarType *t)
{
    OscarType *res = new OscarType();

    ForEach([&f, &res](const OscarType &v) {
        res = res->push_front(f(v));
    }, t);

    return Reverse(res);
}

template <typename F>
auto Map(F f, set t) -> immut::set<decltype(f(t.front()))>
{
    using U = decltype(f(t.front()));

    static_assert(is_convertible<F, function<U(OscarType)>>::value,
        "Met requires a function OscarType U(OscarType)");

    immut::set<U> res;

    t.forEach([&f, &res](const OscarType &v) { res = Put<U>(f(v), res); });

    return res;
}

template <typename F>
auto Map(F f, map t) -> map<OscarType, decltype(f(t.rootKey(), t.rootValue()))>
{
    using U = decltype(f(t.rootKey(), t.rootValue()));

    static_assert(is_convertible<F, function<U(OscarType, OscarType)>>::value,
        "Map requires a function OscarType U(OscarType, OscarType)");

    immut::map<OscarType, U> res;

    t.forEach([&f, &res](OscarType const &k, OscarType const &v) {
        res = Put(k, f(k, v), res);
    });

    return res;
}

template <typename F>
OscarType Reduce(F f, list lst)
{
    auto next = lst.pop_front();

    return FoldLeft(f, f(lst.front(), next.front()), next.pop_front());
}

list MergeFront(list const &a, list const &b)
{
    if (a.isEmpty())
        return b;

    return list(a.front(), MergeFront(a.pop_front(), b));
}

list MergeBack(list const &a, list const &b)
{
    return MergeFront(b, a);
}

list PopFront(list const l)
{
    return l.pop_front();
}

list PopBack(list const l)
{
    size_t size = Size(l) - 1;

    return l.removeAt(0, size);
}

list PushFront(OscarType t, list const l)
{
    return l.push_front(t);
}

list PushBack(OscarType t, list const l)
{
    auto r1 = Reverse(l);
    auto r2 = r1.push_front(t);

    return Reverse(r2);
}

set Union(set const & a, set const & b)
{
    set res = a;

    ForEach([&res, &a](const OscarType &v){
        if (!a.contains(v))
            res = res.inserted(v);
    }, b);

    return res;
}

set Diff(set const & a, set const & b)
{
    set res;

    ForEach([&res, &a](const OscarType &v){
        if (!a.contains(v))
            res = res.inserted(v);
    }, b);

    return res;
}

set Intersection(const set &a, const set &b)
{
    set res;

    ForEach([&res, &a](const OscarType &v){
        if (a.contains(v))
            res = res.inserted(v);
    }, b);

    return res;
}

bool SubSet(const set &a, const set &b)
{
    bool is_subset = true;

    ForEach([&is_subset, &b](const OscarType &v) {
        if (!b.contains(v))
            is_subset = false;
    }, a);

    return is_subset;
}

/*
void test_list(void)
{
    immut::list<int> lst = { 1, 2, 3, 4, 5, 6, 7, 8, 9};

    cout << lst << endl;

    auto mapped = Map([](int x) -> immut::list<double> {
        return immut::list<double>{ x + 0.5 };
    }, lst);

    cout << mapped << endl;

    auto fold = FoldLeft([](double acc, int x) -> double {
        return acc * x;
    }, 10.0, lst);

    cout << fold << endl;

    auto filter = Filter([](int x) -> bool {
        return x % 2 == 0;
    }, lst);

    cout << filter << endl;

    auto mergeF = MergeFront(lst, filter);
    auto mergeB = MergeBack(lst, filter);

    cout << mergeF << endl;
    cout << mergeB << endl;

    auto pushF = PushFront(0, lst);
    auto pushB = PushBack(0, lst);

    auto popF = PopFront(lst);
    auto popB = PopBack(lst);

    cout << pushF << endl;
    cout << pushB << endl;

    cout << popF << endl;
    cout << popB << endl;

    cout << Contains(1, lst) << endl;
    cout << Contains(11, lst) << endl;
    cout << Size(lst) << endl;
    cout << lst << endl << endl;
}

void test_set(void)
{
    immut::set<int> set = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    immut::set<int> set2 = {4, 5, 6, 7, 8, 9, 9, 10, 11, 12};
    immut::set<int> set3 = {1, 2, 3, 4, 5, 6, 9};

    cout << set << endl;
    cout << set2 << endl;

    auto mapped = Map([](int x) -> immut::list<double> {
        return immut::list<double>{ x + 0.5 };
    }, set);

    cout << mapped << endl;

    auto filter = Filter([](int x) -> bool {
        return x % 2 == 0;
    }, set);

    cout << filter << endl;

    auto uni = Union(set, set2);
    auto diff = Diff(set, set2);
    auto intersect = Intersection(set, set2);

    cout << uni << endl;
    cout << diff << endl;
    cout << intersect << endl;
    cout << SubSet(set, set2) << endl;
    cout << SubSet(set, set3) << endl;
    cout << SubSet(set3, set) << endl;
    cout << Put(12, set) << endl;
    cout << Size(set) << endl;
    cout << set << endl << endl;
}

void test_map(void)
{
    immut::map<string, int> map = { {"a", 1}, {"b", 2}, {"c", 3}, {"d", 4} };

    auto mapped = Map([](string s, int x) -> immut::list<double> {
        return immut::list<double> { x + 0.5, 4.5 };
    }, map);

    cout << mapped << endl;

    auto filter = Filter([](string s, int x) -> bool {
        return x % 2 == 0;
    }, map);

    cout << filter << endl;
    cout << Contains(string("b"), map) << endl;
    cout << Contains(string("z"), map) << endl;
    cout << Put(string("h"), 12, map) << endl;
    cout << Size(map) << endl;
    cout << map << endl;
}

int main(void)
{
    test_list();
    test_set();
    test_map();

    return 0;
}*/
