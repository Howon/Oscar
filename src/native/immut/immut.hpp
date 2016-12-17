#ifndef __IMMUT_HPP__
#define __IMMUT_HPP__

#include "list.hpp"
#include "map.hpp"
#include "set.hpp"

#include <iomanip>
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

int Size(string s)
{
    return s.size();
}

template <typename T>
int Size(immut::list<T> l)
{
    return FoldLeft([](size_t acc, T t) -> size_t {
        return acc + 1;
    }, 0, l);
}

template <typename T>
int Size(immut::set<T> s)
{
    size_t size = 0;

    ForEach([&size](T t) {
        size++;
    }, s);

    return size;
}

template <typename K, typename V>
int Size(immut::map<K, V> m)
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
immut::list<T> Prepend(T t, immut::list<T> const l)
{
    return l.push_front(t);
}

template <typename T>
immut::list<T> Append(T t, immut::list<T> const l)
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

    ForEach([&res, &b](const T &v){
        if (!b.contains(v))
            res = res.inserted(v);
    }, a);

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

int AsInt(double d) { return int(d); }

double AsDouble(int i) { return double(i); }

std::string AsString(int i) { return std::to_string(i); }

std::string AsString(char c) { return std::to_string(c); }

std::string AsString(bool b) { return std::to_string(b); }

std::string AsString(double d) { return std::to_string(d); }

template <typename T>
std::string AsString(immut::list<T> l) { return l.str(); }

template <typename T>
std::string AsString(immut::set<T> s) { return s.str(); }

template <typename K, typename V>
std::string AsString(immut::map<K, V> m) { return m.str(); }

void Println(int i) { std::cout << i << std::endl; }

void Println(char c) { std::cout << c << std::endl; }

void Println(bool b) {
    std::cout << (b == true ? "true" : "false") << std::endl;
}

void Println(double d) {
    std::cout << fixed << setprecision(10);
    std::cout << d << std::endl;
}

void Println(string s) { std::cout << s << std::endl; }

template <typename T>
void Println(immut::list<T> l) { std::cout << l << std::endl; }

template <typename T>
void Println(immut::set<T> s) { std::cout << s << std::endl; }

template <typename K, typename V>
void Println(immut::map<K, V> m) { std::cout << m << std::endl; }

#endif
