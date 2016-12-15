#include "list.h"
#include "map.h"
#include "set.h"

template <typename T>
immut::collection<T> factory(immut::Type type)
{
    if (type == immut::L)
        return immut::list<T>();

    return immut::set<T>();
}

template <typename T, typename F>
void ForEach(const F f, const immut::collection<T> &t)
{
    auto col = factory<T>(t.getType());

    col = t;

    col.forEach(f);
}

template <typename K, typename V, typename F>
void ForEach(const F f, const immut::map<K, V> &t)
{
    t.forEach(f);
}

template <typename T, typename F>
immut::collection<T> Filter(F f, immut::collection<T> t)
{
    static_assert(is_convertible<F, function<bool(T)>>::value,
        "Filter requires a function type bool(T)");

    auto res = factory(t.getType());

    ForEach([&f, &res, &t](T const &v){
        if (p(v))
            res = Put(v, res);
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
        if (f(v, k))
            res = Put(k, v, res);
    }, t);

    return res;
}

template<class T, class F>
auto Map(F f, immut::collection<T> t) ->
    immut::collection<decltype(f(t.front()))>
{
    using U = decltype(f(t.front()));

    static_assert(is_convertible<F, function<U(T)>>::value,
        "immut::map requires a function type U(T)");

    auto res = factory(t.getType());

    t.forEach([&f, &res](const T &v){ res = Put(v, res); });

    return res;
}

template <typename K, typename V, typename F>
auto Map(F f, immut::map<K, V> t) ->
    immut::map<K, decltype(f(t.front(), t.rootValue()))>
{
    using U = decltype(f(t.front(), t.rootValue()));

    static_assert(is_convertible<F, function<U(K, V)>>::value,
        "immut::map requires a function type U(K, V)");

    immut::map<K, U> res;

    t.forEach([&f, &res](K const &k, V const &v){
        res = Put(k, f(k, v), res);
    });

    return res;
}

template <typename T>
immut::set<T> Put(T x, immut::set<T> s)
{
    auto t = s.insert(x);

    return set(immut::B, t.left(), t.front(), t.right());
}

template <typename K, typename V>
immut::map<K, V> Put(K k, V v, immut::map<K, V> m)
{
    auto t = m.insert(k, v);

    return map(immut::B, t.left(), t.front(), t.right());
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

template<class T, class U, class F>
U Reduce(F f, immut::list<T> lst)
{
    static_assert(is_convertible<F, function<U(U, T)>>::value,
        "Reduce requires a function type U(U, T)");
    static_assert(!lst.isEmpty(),
        "Reduce requires a nonempty collection");

    auto next = lst.popped_front;

    return FoldLeft(f, f(lst.front(), next.front()), next.popped_front());
}

template<class T>
immut::list<T> MergeFront(immut::list<T> const &a, immut::list<T> const &b)
{
    if (a.isEmpty())
        return b;

    return immut::list<T>(a.front(), MergeFront(a.popped_front(), b));
}

template<class T>
immut::list<T> MergeBack(immut::list<T> const &a, immut::list<T> const &b)
{
    return MergeFront(b, a);
}

template<class T>
immut::list<T> Reverse(immut::list<T> const &lst)
{
    return FoldLeft([](immut::list<T> const &acc, T v) {
        return immut::list<T>(v, acc);
    }, immut::list<T>(), lst);
}

template<class T>
immut::set<T> Union(immut::set<T> const & a, immut::set<T> const & b)
{
    immut::set<T> res = a;

    ForEach([&res, &a](const T &v){
        if (!a.member(v))
            res.Put(v);
    }, b);

    return res;
}

template<class T>
immut::set<T> Diff(immut::set<T> const & a, immut::set<T> const & b)
{
    immut::set<T> res;

    ForEach([&res, &a](const T &v){
        if (!a.member(v))
            res.Put(v);
    }, b);

    return res;
}

template<class T>
immut::set<T> Intersection(immut::set<T> const & a, immut::set<T> const & b)
{
    immut::set<T> res;

    ForEach([&res, &a](const T &v){
        if (a.member(v))
            res.Put(v);
    }, b);

    return res;
}

int main(void)
{
    immut::list<int> lst = { 1, 2, 3 };
cout << "what" << endl;
    ForEach([](int x){
        cout << "waht" << endl;
        cout << x << endl;
    }, lst);

    return 0;
}
