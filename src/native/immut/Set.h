#ifndef __SET_H__
#define __SET_H__

#include <iostream>
#include <cassert>
#include <memory>
#include <functional>

#include "collection.h"

using namespace std;

namespace immut {
    template <typename T>
    class set : public collection<T> {
        Type type = S;

        struct Node {
            Node(Color c, shared_ptr<const Node> const &lft, T val,
                shared_ptr<const Node> const & rgt) :
                _hash(rand()), _c(c), _lft(lft), _val(val), _rgt(rgt) {}

            int _hash;
            Color _c;
            T _val;

            shared_ptr<const Node> _lft;
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
            return !this->isEmpty() && this->rootColor() == R &&
                !this->left().isEmpty() && this->left().rootColor() == R;
        }

        bool doubledRight() const
        {
            return !this->isEmpty() && this->rootColor() == R &&
                !this->right().isEmpty() && this->right().rootColor() == R;
        }

        set balance(set const & lft, T x, set const & rgt) const
        {
            if (lft.doubledLeft()) {
                return set(R, lft.left().paint(B), lft.front(),
                    set(B, lft.right(), x, rgt));
            } else if (lft.doubledRight()) {
                return set(R, set(B, lft.left(), lft.front(),
                    lft.right().left()), lft.right().front(),
                    set(B, lft.right().right(), x, rgt));
            } else if (rgt.doubledLeft()) {
                return set(R, set(B, lft, x, rgt.left().left()),
                    rgt.left().front(), set(B, rgt.left().right(),
                    rgt.front(), rgt.right()));
            } else if (rgt.doubledRight()) {
                return set(R, set(B, lft, x, rgt.left()),
                    rgt.front(), rgt.right().paint(B));
            } else
                return set(B, lft, x, rgt);
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
        set() {}

        set(Color c, const set &lft, T val, const set &rgt)
            : _root(make_shared<const Node>(c, lft._root, val, rgt._root))
        {
            assert(lft.isEmpty() || lft.front() < val);
            assert(rgt.isEmpty() || val < rgt.front());
        }

        set<T> (initializer_list<T> init)
        {
            set<T> t;

            for (T v : init)
                t = t.inserted(v);

            this->_root = t.get_root();
        }

        shared_ptr<const Node> get_root() { return _root; }

        T front() const { return _root->_val; }

        bool isEmpty() const { return !_root; }

        template <typename F>
        void forEach(F f) const {
            static_assert(is_convertible<F, function<void(T)>>::value,
                 "ForEach requires a function type void(T)");

            if (!this->isEmpty()) {
                this->left().forEach(f);

                f(this->front());

                this->right().forEach(f);
            }
        }

        set<T> insert(T x) const
        {
            this->assert1();

            if (this->isEmpty())
                return set(R, set(), x, set());

            T y = this->front();

            if (x == y)
                return *this;

            Color c = this->rootColor();

            if (this->rootColor() == B) {
                if (x < y)
                    return balance(this->left().insert(x), y, this->right());

                return balance(this->left(), y, this->right().insert(x));
            } else {
                if (x < y)
                    return set(c, this->left().insert(x), y, this->right());

                return set(c, this->left(), y, this->right().insert(x));
            }
        }

        set inserted(T x) const
        {
          auto s = insert(x);

          return set(B, s.left(), s.front(), s.right());
        }

        virtual bool contains(T x) const
        {
            if (this->isEmpty())
                return false;

            T y = this->front();

            if (x < y)
                return left().contains(x);
            else if (y < x)
                return right().contains(x);
            else
                return true;
        }

        bool operator==(const set &rhs)
        {
            forEach([&rhs](T t) {
                if (!rhs.contains(t))
                    return false;
            });

            return true;
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

            s.forEach([&os](T t) {
                os << t << " ";
            });

            os << "]";

            return os;
        }
    };
}

#endif
