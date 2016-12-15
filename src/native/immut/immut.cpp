#include "list.h"
#include "map.h"
#include "set.h"

#include <vector>
#include <string>

template <typename T, typename F>
void ForEach(const F f, const immut::list<T> &t)
{
    t.forEach(f);
}

template <typename T, typename F>
void ForEach(const F f, const immut::set<T> &t)
{
    t.forEach(f);
}

template <typename K, typename V, typename F>
void ForEach(const F f, const immut::map<K, V> &t)
{
    t.forEach(f);
}

template<class T, class U, class F>
U FoldLeft(F f, U acc, immut::list<T> lst)
{
    static_assert(is_convertible<F, function<U(U, T)>>::value,
        "FoldLeft requires a function type U(U, T)");

    ForEach([&f, &acc](const T &v){
        acc = f(acc, v);
    }, lst);

    return acc;
}

template<class T>
immut::list<T> Reverse(immut::list<T> const &lst)
{
    return FoldLeft([](immut::list<T> const &acc, T v) {
        return immut::list<T>(v, acc);
    }, immut::list<T>(), lst);
}

template <typename T>
size_t Size(immut::list<T> l)
{
    return FoldLeft([](size_t acc, T t) -> size_t {
        return acc + 1;
    }, 0, l);
}

template <typename T>
size_t Size(immut::set<T> s)
{
    size_t size = 0;

    ForEach([&size](T t) {
        size++;
    }, s);

    return size;
}

template <typename K, typename V>
size_t Size(immut::map<K, V> m)
{
    size_t size = 0;

    ForEach([&size](K k, V v) {
        size++;
    }, m);

    return size;
}


template <typename T>
immut::set<T> Put(T x, immut::set<T> s)
{
    return s.inserted(x);
}

template <typename K, typename V>
immut::map<K, V> Put(K k, V v, immut::map<K, V> m)
{
    return m.inserted(k, v);
}

template <typename T, typename F>
immut::list<T> Filter(F f, immut::list<T> t)
{
    static_assert(is_convertible<F, function<bool(T)>>::value,
        "Filter requires a function type bool(T)");

    immut::list<T> res;

    ForEach([&f, &res, &t](T const &v){
        if (f(v))
            res = res.push_front(v);
    }, t);

    return Reverse<T>(res);
}

template <typename T, typename F>
immut::set<T> Filter(F f, immut::set<T> t)
{
    static_assert(is_convertible<F, function<bool(T)>>::value,
        "Filter requires a function type bool(T)");

    immut::set<T> res;

    ForEach([&f, &res, &t](T const &v){
        if (f(v))
            res = Put(v, res.inserted(v));
    }, t);

    return res;
}

template <typename K, typename V, typename F>
immut::map<K, V> Filter(F f, immut::map<K, V> t)
{
    static_assert(is_convertible<F, function<bool(K, V)>>::value,
        "Filter requires a function type bool(K, V)");

    immut::map<K, V> res;

    ForEach([&f, &res, &t](const K &k, const V &v){
        if (f(k, v))
            res = Put(k, v, res);
    }, t);

    return res;
}

template <typename T>
bool Contains(T t, immut::list<T> l)
{
    return l.contains(t);
}

template <typename T>
bool Contains(T t, immut::set<T> s)
{
    return s.contains(t);
}

template <typename K, typename V>
bool Contains(K k, immut::map<K, V> m)
{
    return m.contains(k);
}

template<class T, class F>
auto Map(F f, immut::list<T> t) -> immut::list<decltype(f(t.front()))>
{
    using U = decltype(f(t.front()));

    static_assert(is_convertible<F, function<U(T)>>::value,
        "Map requires a function type U(T)");

    immut::list<U> res;

    t.forEach([&f, &res](const T &v) { res = res.push_front(f(v)); });

    return Reverse<U>(res);
}

template<class T, class F>
auto Map(F f, immut::set<T> t) -> immut::set<decltype(f(t.front()))>
{
    using U = decltype(f(t.front()));

    static_assert(is_convertible<F, function<U(T)>>::value,
        "Met requires a function type U(T)");

    immut::set<U> res;

    t.forEach([&f, &res](const T &v) { res = Put<U>(f(v), res); });

    return res;
}

template <typename K, typename V, typename F>
auto Map(F f, immut::map<K, V> t) ->
    immut::map<K, decltype(f(t.rootKey(), t.rootValue()))>
{
    using U = decltype(f(t.rootKey(), t.rootValue()));

    static_assert(is_convertible<F, function<U(K, V)>>::value,
        "Map requires a function type U(K, V)");

    immut::map<K, U> res;

    t.forEach([&f, &res](K const &k, V const &v) {
        res = Put(k, f(k, v), res);
    });

    return res;
}

template <typename T, typename U, typename F>
U Reduce(F f, immut::list<T> lst)
{
    static_assert(is_convertible<F, function<U(T, T)>>::value,
        "Reduce requires a function type U(T, T)");
    static_assert(!lst.isEmpty(),
        "Reduce requires a nonempty collection");

    auto next = lst.popped_front;

    return FoldLeft(f, f(lst.front(), next.front()), next.popped_front());
}

template <typename T>
immut::list<T> MergeFront(immut::list<T> const &a, immut::list<T> const &b)
{
    if (a.isEmpty())
        return b;

    return immut::list<T>(a.front(), MergeFront(a.pop_front(), b));
}

template <typename T>
immut::list<T> MergeBack(immut::list<T> const &a, immut::list<T> const &b)
{
    return MergeFront(b, a);
}

template <typename T>
immut::list<T> PopFront(immut::list<T> const l)
{
    return l.pop_front();
}

template <typename T>
immut::list<T> PopBack(immut::list<T> const l)
{
    size_t size = Size(l) - 1;

    return l.removeAt(0, size);
}

template <typename T>
immut::list<T> PushFront(T t, immut::list<T> const l)
{
    return l.push_front(t);
}

template <typename T>
immut::list<T> PushBack(T t, immut::list<T> const l)
{
    auto r1 = Reverse(l);
    auto r2 = r1.push_front(t);

    return Reverse(r2);
}

template <typename T>
immut::set<T> Union(immut::set<T> const & a, immut::set<T> const & b)
{
    immut::set<T> res = a;

    ForEach([&res, &a](const T &v){
        if (!a.contains(v))
            res = res.inserted(v);
    }, b);

    return res;
}

template<class T>
immut::set<T> Diff(immut::set<T> const & a, immut::set<T> const & b)
{
    immut::set<T> res;

    ForEach([&res, &a](const T &v){
        if (!a.contains(v))
            res = res.inserted(v);
    }, b);

    return res;
}

template<class T>
immut::set<T> Intersection(immut::set<T> const & a, immut::set<T> const & b)
{
    immut::set<T> res;

    ForEach([&res, &a](const T &v){
        if (a.contains(v))
            res = res.inserted(v);
    }, b);

    return res;
}

template<class T>
bool SubSet(immut::set<T> const & a, immut::set<T> const & b)
{
    bool is_subset = true;

    ForEach([&is_subset, &b](const T &v) {
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
}
*/
