#ifndef __MAP_H__
#define __MAP_H__

#include <vector>
#include "collection.h"

using namespace std;

namespace immut {
    template <typename K, typename V>
    class map : public collection<K>
    {
        Type type = M;

        struct KVNode
        {
            KVNode(Color c, shared_ptr<const KVNode> const &lft,
                K key, V val, shared_ptr<const KVNode> const & rgt)
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
            K _key;
            V _val;

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

        int countB() const
        {
            if (this->isEmpty())
                return 0;

            int lft = left().countB();

            assert(lft == right().countB());

            return (this->rootColor() == B)? 1 + lft: lft;
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
            return !this->isEmpty() && this->rootColor() == R &&
                !this->left().isEmpty() && this->left().rootColor() == R;
        }

        bool doubledRight() const
        {
            return !this->isEmpty() && this->rootColor() == R &&
                !this->right().isEmpty() && this->right().rootColor() == R;
        }

        map balance(const map &lft, K x, V v, const map &rgt) const
        {
            if (lft.doubledLeft()) {
                return map(R, lft.left().paint(B), lft.rootKey(),
                    lft.rootValue(), map(B, lft.right(), x, v, rgt));
            } else if (lft.doubledRight()) {
                return map(R, map(B, lft.left(), lft.rootKey(), lft.rootValue(),
                    lft.right().left()), lft.right().rootKey(),
                    lft.right().rootValue(), map(B, lft.right().right(),
                    x, v, rgt));
            } else if (rgt.doubledLeft()) {
                return map(R, map(B, lft, x, v, rgt.left().left()),
                    rgt.left().rootKey(), rgt.left().rootValue(),
                    map(B, rgt.left().right(), rgt.rootKey(),
                    rgt.rootValue(), rgt.right()));
            } else if (rgt.doubledRight()) {
                return map(R, map(B, lft, x, v, rgt.left()), rgt.rootKey(),
                    rgt.rootValue(), rgt.right().paint(B));
            } else
                return map(B, lft, x, v, rgt);
        }

        virtual void assert1() const
        {
            if (!this->isEmpty()) {
                auto lft = left();
                auto rgt = right();

                if (this->rootColor() == R) {
                    assert(lft.isEmpty() || lft.rootColor() == B);
                    assert(rgt.isEmpty() || rgt.rootColor() == B);
                }

                lft.assert1();
                rgt.assert1();
            }
        }

    public:
        map() {}

        map(Color c, const map &lft, K key, V val, const map &rgt) :
            _root(make_shared<const KVNode>(c, lft._root, key, val, rgt._root))
        {
            assert(lft.isEmpty() || lft.rootKey() < key);
            assert(rgt.isEmpty() || key < rgt.rootKey());
        }

        map(initializer_list<pair<K, V> > initMap)
        {
            map<K, V> m;

            for (auto it = initMap.begin(); it != initMap.end(); ++it)
                m = m.inserted(it->first, it->second);

            this->_root = m.get_root();
        }

        shared_ptr<const KVNode> get_root() { return _root; }

        K rootKey() const
        {
            assert(!this->isEmpty());

            return _root->_key;
        }

        V rootValue() const
        {
            assert(!this->isEmpty());

            return _root->_val;
        }

        bool isEmpty() const { return !_root; }

        template <typename F>
        void forEach(F f)  const
        {
            static_assert(is_convertible<F, function<void(K, V)>>::value,
                 "ForEach requires a function type void(K, V)");

            if (!this->isEmpty()) {
                this->left().forEach(f);

                f(this->rootKey(), rootValue());

                this->right().forEach(f);
            }
        }

        map insert(K x, V v) const
        {
            this->assert1();

            if (this->isEmpty())
                return map(R, map(), x, v, map());

            K y = this->rootKey();
            V yv = this->rootValue();

            if (x == y)
                return map(B, this->left(), x, yv, this->right());

            Color c = this->rootColor();

            if (this->rootColor() == B) {
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

        map  inserted(K x, V v) const
        {
            auto t = insert(x, v);

            return map(B, t.left(), t.rootKey(), t.rootValue(), t.right());
        }

        virtual bool contains(K x) const
        {
            if (this->isEmpty())
                return false;

            K y = this->rootKey();

            if (x < y)
                return left().contains(x);
            else if (y < x)
                return right().contains(x);
            else
                return true;
        }

        V find(K key) const
        {
            if (this->isEmpty())
                throw out_of_range("Key not found");

            K y = this->rootKey();

            if (key < y)
                return left().find(key);
            else if (y < key)
                return right().find(key);
            else
                return rootValue();
        }

        bool operator==(const map &rhs)
        {
            forEach([&rhs](K k, V v) {
                if (!rhs.contains(k) || rhs.find(k) != v)
                    return false;
            });

            return true;
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

            m.forEach([&os](K k, V v) {
                os << "(" << k << " -> " << v << ") ";
            });

            os << "]";

            return os;
        }
    };
}

#endif
