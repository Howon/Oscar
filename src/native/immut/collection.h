#ifndef __COLLECTION_H__
#define __COLLECTION_H__

#include <iostream>
#include <atomic>
#include <memory>
#include <stdexcept>
#include <cassert>
#include <functional>
#include <initializer_list>

#include <stdlib.h>
#include <time.h>

using namespace std;

namespace immut {
    enum Type { I, C, D, B, S, CL, CS, CM };
    enum Color { CR, CB };

    class list;
    class set;
    class map;
    class OscarType;

    bool OscarTypeEquals(const OscarType &lhs, const OscarType &rhs);
    bool OscarTypeLt(const OscarType &lhs, const OscarType &rhs);
    bool OscarTypeGt(const OscarType &lhs, const OscarType &rhs);

    class OscarType {
    public :
        Type t;
        int i;
        char c;
        double d;
        bool b;
        string s;
        list *cl;
        set *cs;
        map *cm;

        OscarType() {}
        OscarType(int n) : t(I), i(n) {}
        OscarType(char n) : t(C), c(n) {}
        OscarType(double n) : t(D), d(n) {}
        OscarType(bool n) : t(B), b(n) {}
        OscarType(string n) : t(S), s(n) {}
        OscarType(list *n) : t(CL), cl(n) {}
        OscarType(set *n) : t(CS), cs(n) {}
        OscarType(map *n) : t(CM), cm(n) {}

        bool operator==(const OscarType &rhs)
        {
            return OscarTypeEquals(*this, rhs);
        }

        bool operator!=(const OscarType &rhs)
        {
            return !OscarTypeEquals(*this, rhs);
        }

        bool operator > (const OscarType &rhs)
        {
            return OscarTypeLt(*this, rhs);
        }

        bool operator < (const OscarType &rhs)
        {
             return OscarTypeGt(*this, rhs);
        }

        friend ostream &operator<<(ostream &os, const OscarType &t)
        {
            switch (t.t) {
                case I : return os << t.i;
                case C : return os << t.c;
                case D : return os << t.d;
                case B : return os << t.b;
                case S : return os << t.s;
                case CL : return os << t.cl;
                case CS : return os << t.cs;
                case CM : return os << t.cm;
            }
        }
    };

    class list {
        struct Item {
            Item(OscarType v, shared_ptr<const Item> tail) :
                _hash(rand()), _val(v), _next(move(tail)) {}

            explicit Item(OscarType v) : _val(v) {}

            int _hash;
            OscarType _val;

            shared_ptr<const Item> _next;
        };

        friend Item;

        shared_ptr<const Item> _head;

        explicit list(shared_ptr<const Item> items) : _head(move(items)) {}

    public:
        list() {}

        list(OscarType v, list const &tail) :
            _head(make_shared<Item>(v, tail._head)) {}

        explicit list(OscarType v) : _head(make_shared<Item>(v)) {}

        list(initializer_list<OscarType> init)
        {
            for (auto it = rbegin(init); it != rend(init); ++it) {
                _head = make_shared<Item>(*it, _head);
            }
        }

        shared_ptr<const Item> get_root() const { return _head; }

        bool isEmpty() const { return !_head; }

        OscarType front() const
        {
            assert(!isEmpty());

            return _head->_val;
        }

        list pop_front() const
        {
            assert(!isEmpty());

            return list(_head->_next);
        }

        list push_front(OscarType v) const { return list(v, *this); }

        list take(int n)
        {
            if (n <= 0 || isEmpty())
              return list();

            return pop_front().take(n - 1).push_front(front());
        }

        list insertedAt(int i, OscarType v) const
        {
            if (i == 0)
                return push_front(v);
            else {
                assert(!isEmpty());

                return list(front(), pop_front().insertedAt(i - 1, v));
            }
        }

        list removeAt(int i, int j) const
        {
            if (isEmpty())
              return list();

            if (i == j)
                return pop_front();

            return list(front(), pop_front().removeAt(i + 1, j));
        }

        list removed(OscarType v) const
        {
            if (isEmpty())
              return list();

            if (v == front())
                return pop_front();

            return list(front(), pop_front().removed(v));
        }

        bool contains(OscarType v) const
        {
            if (isEmpty())
              return false;

            if (v == front())
              return true;

            return pop_front().contains(v);
        };

        template <typename F>
        void forEach(const F f) const
        {
            Item const *it = get_root().get();

            while (it != nullptr) {
                f(it->_val);

                it = it->_next.get();
            }
        }

        bool operator==(const list &rhs)
        {
            const Item *it1 = get_root().get();
            const Item *it2 = rhs.get_root().get();

            while (!(it1 == nullptr || it2 == nullptr)) {
                if (!OscarTypeEquals(it1->_val, it2->_val))
                    return false;

                it1 = it1->_next.get();
                it2 = it2->_next.get();
            }

            if ((it1 == nullptr && it2 != nullptr) ||
                  (it1 != nullptr && it2 == nullptr))
              return false;

            return true;
        }

        bool operator > (const list &rhs)
        {
            return get_root().get()->_hash > rhs.get_root().get()->_hash;
        }

        bool operator < (const list &rhs)
        {
            return get_root().get()->_hash < rhs.get_root().get()->_hash;
        }

        friend ostream &operator<<(ostream &os, const list &t)
        {
            os << "[ ";

            t.forEach([&os](OscarType v) {
                os << v << " ";
            });

            os << "]";

            return os;
        }
    };

    class set {
        struct Node {
            Node(Color c, shared_ptr<const Node> const &lft, OscarType val,
                shared_ptr<const Node> const &rgt) :
                _hash(rand()), _c(c), _lft(lft), _val(val), _rgt(rgt) {}

            int _hash;
            Color _c;

            shared_ptr<const Node> _lft;

            OscarType _val;

            shared_ptr<const Node> _rgt;
        };

        explicit set(const shared_ptr<const Node> &node) : _root(node) {}

        shared_ptr<const Node> _root;

        Color rootColor() const
        {
            assert(!isEmpty());

            return _root->_c;
        }

        set paint(Color c) const
        {
            assert(!this->isEmpty());

            return set(c, left(), this->front(), right());
        }

        set left() const
        {
            assert(!this->isEmpty());

            return set(this->_root->_lft);
        }

        set right() const
        {
            assert(!this->isEmpty());

            return set(this->_root->_rgt);
        }

        bool doubledLeft() const
        {
            return !this->isEmpty() && this->rootColor() == CR &&
                !this->left().isEmpty() && this->left().rootColor() == CR;
        }

        bool doubledCRight() const
        {
            return !this->isEmpty() && this->rootColor() == CR &&
                !this->right().isEmpty() && this->right().rootColor() == CR;
        }

        set balance(set const & lft, OscarType x, set const & rgt) const
        {
            if (lft.doubledLeft()) {
                return set(CR, lft.left().paint(CB), lft.front(),
                    set(CB, lft.right(), x, rgt));
            } else if (lft.doubledCRight()) {
                return set(CR, set(CB, lft.left(), lft.front(),
                    lft.right().left()), lft.right().front(),
                    set(CB, lft.right().right(), x, rgt));
            } else if (rgt.doubledLeft()) {
                return set(CR, set(CB, lft, x, rgt.left().left()),
                    rgt.left().front(), set(CB, rgt.left().right(),
                    rgt.front(), rgt.right()));
            } else if (rgt.doubledCRight()) {
                return set(CR, set(CB, lft, x, rgt.left()),
                    rgt.front(), rgt.right().paint(CB));
            } else
                return set(CB, lft, x, rgt);
        }

        void assert1() const
        {
            if (!this->isEmpty()) {
                auto lft = left();
                auto rgt = right();

                if (this->rootColor() == CR) {
                    assert(lft.isEmpty() || lft.rootColor() == CB);
                    assert(rgt.isEmpty() || rgt.rootColor() == CB);
                }

                lft.assert1();
                rgt.assert1();
            }
        }

    public:
        set() {}

        set(Color c, const set &lft, OscarType val, const set &rgt)
            : _root(make_shared<const Node>(c, lft._root, val, rgt._root))
        {
            assert(lft.isEmpty() || lft.front() < val);
            assert(rgt.isEmpty() || val < rgt.front());
        }

        set (initializer_list<OscarType> init)
        {
            set t;

            for (OscarType v : init)
                t = t.inserted(v);

            this->_root = t.get_root();
        }

        shared_ptr<const Node> get_root() const { return _root; }

        OscarType front() const { return _root->_val; }

        bool isEmpty() const { return !_root; }

        template <typename F>
        void forEach(F f) const {
            static_assert(is_convertible<F, function<void(OscarType)>>::value,
                 "ForEach requires a function OscarType void(T)");

            if (!this->isEmpty()) {
                this->left().forEach(f);

                f(this->front());

                this->right().forEach(f);
            }
        }

        set insert(OscarType x) const
        {
            this->assert1();

            if (this->isEmpty())
                return set(CR, set(), x, set());

            OscarType y = this->front();

            if (x == y)
                return *this;

            Color c = this->rootColor();

            if (this->rootColor() == CB) {
                if (x < y)
                    return balance(this->left().insert(x), y, this->right());

                return balance(this->left(), y, this->right().insert(x));
            } else {
                if (x < y)
                    return set(c, this->left().insert(x), y, this->right());

                return set(c, this->left(), y, this->right().insert(x));
            }
        }

        set inserted(OscarType x) const
        {
          auto s = insert(x);

          return set(CB, s.left(), s.front(), s.right());
        }

        bool contains(OscarType x) const
        {
            if (this->isEmpty())
                return false;

            OscarType y = this->front();

            if (x < y)
                return left().contains(x);
            else if (y < x)
                return right().contains(x);
            else
                return true;
        }

        bool operator==(const set &rhs)
        {
            bool return_v = true;

            forEach([&return_v, &rhs](OscarType t) {
                if (!rhs.contains(t))
                    return_v = false;
            });

            return return_v;
        }

        bool operator > (const set &rhs)
        {
            return get_root().get()->_hash > rhs.get_root().get()->_hash;
        }

        bool operator < (const set &rhs)
        {
            return get_root().get()->_hash < rhs.get_root().get()->_hash;
        }

        friend ostream &operator<<(std::ostream& os, const set &s)
        {
            os << "[ ";

            s.forEach([&os](OscarType t) {
                os << t << " ";
            });

            os << "]";

            return os;
        }
    };

    class map {
        struct KVNode
        {
            KVNode(Color c, shared_ptr<const KVNode> const &lft,
                OscarType key, OscarType val, shared_ptr<const KVNode> const &rgt)
            {
                _hash = rand();
                _c = c;
                _lft = lft;
                _rgt = rgt;
                _key = key;
                _val = val;
            }

            int _hash;

            Color _c;
            OscarType _key;
            OscarType _val;

            shared_ptr<const KVNode> _lft;
            shared_ptr<const KVNode> _rgt;
        };

        explicit map(shared_ptr<const KVNode> const &node) {
            _root = node;
        }

        shared_ptr<const KVNode> _root;

        Color rootColor() const
        {
            assert(!isEmpty());

            return _root->_c;
        }

        map paint(Color c) const
        {
            assert(!this->isEmpty());

            return map(c, left(), this->rootKey(), this->rootValue(), right());
        }

        int countCB() const
        {
            if (this->isEmpty())
                return 0;

            int lft = left().countCB();

            assert(lft == right().countCB());

            return (this->rootColor() == CB)? 1 + lft: lft;
        }

        map left() const
        {
            assert(!this->isEmpty());

            return map(_root->_lft);
        }

        map right() const
        {
            assert(!this->isEmpty());

            return map(_root->_rgt);
        }

        bool doubledLeft() const
        {
            return !this->isEmpty() && this->rootColor() == CR &&
                !this->left().isEmpty() && this->left().rootColor() == CR;
        }

        bool doubledCRight() const
        {
            return !this->isEmpty() && this->rootColor() == CR &&
                !this->right().isEmpty() && this->right().rootColor() == CR;
        }

        map balance(const map &lft, OscarType x, OscarType v, const map &rgt) const
        {
            if (lft.doubledLeft()) {
                return map(CR, lft.left().paint(CB), lft.rootKey(),
                    lft.rootValue(), map(CB, lft.right(), x, v, rgt));
            } else if (lft.doubledCRight()) {
                return map(CR, map(CB, lft.left(), lft.rootKey(), lft.rootValue(),
                    lft.right().left()), lft.right().rootKey(),
                    lft.right().rootValue(), map(CB, lft.right().right(),
                    x, v, rgt));
            } else if (rgt.doubledLeft()) {
                return map(CR, map(CB, lft, x, v, rgt.left().left()),
                    rgt.left().rootKey(), rgt.left().rootValue(),
                    map(CB, rgt.left().right(), rgt.rootKey(),
                    rgt.rootValue(), rgt.right()));
            } else if (rgt.doubledCRight()) {
                return map(CR, map(CB, lft, x, v, rgt.left()), rgt.rootKey(),
                    rgt.rootValue(), rgt.right().paint(CB));
            } else
                return map(CB, lft, x, v, rgt);
        }

        void assert1() const
        {
            if (!this->isEmpty()) {
                auto lft = left();
                auto rgt = right();

                if (this->rootColor() == CR) {
                    assert(lft.isEmpty() || lft.rootColor() == CB);
                    assert(rgt.isEmpty() || rgt.rootColor() == CB);
                }

                lft.assert1();
                rgt.assert1();
            }
        }

    public:
        map() {}

        map(Color c, const map &lft, OscarType key, OscarType val, const map &rgt) :
            _root(make_shared<const KVNode>(c, lft._root, key, val, rgt._root))
        {
            assert(lft.isEmpty() || lft.rootKey() < key);
            assert(rgt.isEmpty() || key < rgt.rootKey());
        }

        map(initializer_list<pair<OscarType, OscarType> > initMap)
        {
            map m;

            for (auto it = initMap.begin(); it != initMap.end(); ++it)
                m = m.inserted(it->first, it->second);

            this->_root = m.get_root();
        }

        shared_ptr<const KVNode> get_root() const { return _root; }

        OscarType rootKey() const
        {
            assert(!this->isEmpty());

            return _root->_key;
        }

        OscarType rootValue() const
        {
            assert(!this->isEmpty());

            return _root->_val;
        }

        bool isEmpty() const { return !_root; }

        template <typename F>
        void forEach(F f)  const
        {
            static_assert(is_convertible<F, function<void(OscarType, OscarType)>>::value,
                 "ForEach requires a function OscarType void(K, V)");

            if (!this->isEmpty()) {
                this->left().forEach(f);

                f(this->rootKey(), rootValue());

                this->right().forEach(f);
            }
        }

        map insert(OscarType x, OscarType v) const
        {
            this->assert1();

            if (this->isEmpty())
                return map(CR, map(), x, v, map());

            OscarType y = this->rootKey();
            OscarType yv = this->rootValue();

            if (x == y)
                return map(CB, this->left(), x, yv, this->right());

            Color c = this->rootColor();

            if (this->rootColor() == CB) {
                if (x < y) {
                    return balance(this->left().insert(x, v), y, yv,
                        this->right());
                }

                return balance(this->left(), y, yv, this->right().insert(x, v));
            } else {
                if (x < y) {
                    return map(c, this->left().insert(x, v),
                        y, yv, this->right());
                }

                return map(c, this->left(), y, yv, this->right().insert(x, v));
            }
        }

        map  inserted(OscarType x, OscarType v) const
        {
            auto t = insert(x, v);

            return map(CB, t.left(), t.rootKey(), t.rootValue(), t.right());
        }

        bool contains(OscarType x) const
        {
            if (this->isEmpty())
                return false;

            OscarType y = this->rootKey();

            if (x < y)
                return left().contains(x);
            else if (y < x)
                return right().contains(x);
            else
                return true;
        }

        OscarType find(OscarType key) const
        {
            if (this->isEmpty())
                throw out_of_range("Key not found");

            OscarType y = this->rootKey();

            if (key < y)
                return left().find(key);
            else if (y < key)
                return right().find(key);
            else
                return rootValue();
        }

        bool operator==(const map &rhs)
        {
            bool return_v = true;

            forEach([&return_v, &rhs](OscarType k, OscarType v) {
                if (!rhs.contains(k) || rhs.find(k) != v)
                    return_v = false;
            });

            return return_v;
        }

        bool operator > (const map &rhs)
        {
            return get_root().get()->_hash > rhs.get_root().get()->_hash;
        }

        bool operator < (const map &rhs)
        {
            return get_root().get()->_hash < rhs.get_root().get()->_hash;
        }

        friend ostream &operator<<(ostream &os, const map &m)
        {
            os << "[ ";

            m.forEach([&os](OscarType k, OscarType v) {
                os << "(" << k << " -> " << v << ") ";
            });

            os << "]";

            return os;
        }
    };

    bool OscarTypeEquals(const OscarType &lhs, const OscarType &rhs)
    {
        if (lhs.t != rhs.t)
            return false;

        switch (lhs.t) {
            case I : return lhs.i == rhs.i;
            case C : return lhs.c == rhs.c;
            case D : return lhs.d == rhs.d;
            case B : return lhs.b == rhs.b;
            case S : return lhs.s == rhs.s;
            case CL : return *lhs.cl == *rhs.cl;
            case CS : return *lhs.cs == *rhs.cs;
            case CM : return *lhs.cm == *rhs.cm;
        }

        return true;
    }

    bool OscarTypeLt(const OscarType &lhs, const OscarType &rhs)
    {
        assert(lhs.t == rhs.t);

        switch (lhs.t) {
            case I : return lhs.i > rhs.i;
            case C : return lhs.c > rhs.c;
            case D : return lhs.d > rhs.d;
            case B : return lhs.b == rhs.b;
            case S : return lhs.s > rhs.s;
            case CL : return *lhs.cl > *rhs.cl;
            case CS : return *lhs.cs > *rhs.cs;
            case CM : return *lhs.cm > *rhs.cm;
        }
    }

    bool OscarTypeGt(const OscarType &lhs, const OscarType &rhs)
    {
        assert(lhs.t == rhs.t);

        switch (lhs.t) {
            case I : return lhs.i < rhs.i;
            case C : return lhs.c < rhs.c;
            case D : return lhs.d < rhs.d;
            case B : return lhs.b != rhs.b;
            case S : return lhs.s < rhs.s;
            case CL : return *lhs.cl < *rhs.cl;
            case CS : return *lhs.cs < *rhs.cs;
            case CM : return *lhs.cm < *rhs.cm;
        }
    }
}

#endif
