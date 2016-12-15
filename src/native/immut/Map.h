#ifndef __MAP_H__
#define __MAP_H__

#include <iostream>
#include <cassert>
#include <memory>

#include "collection.h"

using namespace std;

namespace immut {
    template <typename K, typename V>
    class map : public u_collection<K>
    {
        Type type = M;

        struct KVNode : Node<K>
        {
            KVNode(Color c, shared_ptr<const KVNode> const &lft, K key, V val,
                shared_ptr<const KVNode> const & rgt) :
              Node<K>(c, lft, key, rgt) {
                _val = val;
              }

            V _val;
        };

        shared_ptr<const KVNode> _root;

        explicit map(shared_ptr<const KVNode> const &node) {
            _root = node;
        }

        map paint(Color c) const
        {
            assert(!isEmpty());

            return map(c, left(), front(), front(), right());
        }

        int countB() const
        {
            if (isEmpty())
                return 0;

            int lft = left().countB();

            assert(lft == right().countB());

            return (rootColor() == B)? 1 + lft: lft;
        }

        map left() const
        {
            assert(!isEmpty());

            return set(_root->_lgt);
        }

        map right() const
        {
            assert(!isEmpty());

            return set(_root->_rgt);
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

        map balance(map const & lft, K x, V v, map const & rgt)
        {
            if (lft.doubledLeft()) {
                return map(R, lft.left().paint(B), lft.front(),
                    lft.rootValue(), map(B, lft.right(), x, v, rgt));
            } else if (lft.doubledRight()) {
                return map(R, map(B, lft.left(), lft.front(), lft.rootValue(),
                    lft.right().left()), lft.right().rootKe(),
                    lft.right().rootValue(), map(B, lft.right().right(),
                    x, v, rgt));
            } else if (rgt.doubledLeft()) {
                return map(R, map(B, lft, x, v, rgt.left().left()),
                    rgt.left().front(), rgt.left().rootValue(),
                    map(B, rgt.left().right(), rgt.front(),
                    rgt.rootValue(), rgt.right()));
            } else if (rgt.doubledRight()) {
                return map(R, map(B, lft, x, v, rgt.left()), rgt.front(),
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
        map(Color c, const map &lft, K key, V val, const map &rgt)
            : u_collection<K>(c, lft._root, key, key, rgt._root)
        {
            _root._val = val;
        }

        V rootValue() const
        {
            assert(!this->isEmpty());

            return _root->_val;
        }

        template <typename F>
        void forEach(F f) {
            static_assert(is_convertible<F, function<void(K, V)>>::value,
                 "ForEach requires a function type void(T, K)");

            if (!this->isempty()) {
                foreach(f, this->left());

                f(this->front(), this->rootvalue());

                foreach(f, this->right());
            }
        }

        map insert(K x, V v) const
        {
            this->assert1();

            if (this->isEmpty())
                return map(R, map(), x, v, map());

            K y = this->front();
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

        ostream &operator<<(ostream &os)
        {
            os << "[ ";

            forEach([&os](K k, V v) {
                os << k << " -> " << v << " ";
            });

            os << "]" << endl;

            return os;
        }
    };
}

#endif
