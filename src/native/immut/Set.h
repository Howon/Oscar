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
    class set : u_collection<T> {
        Type type = S;

        set left() const
        {
            assert(!isEmpty());

            return set(_root->_lgt);
        }

        set right() const
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

        set balance(set const & lft, T x, set const & rgt)
        {
            if (lft.doubledLeft()) {
                return set(R, lft.left().paint(B), lft.root(),
                    set(B, lft.right(), x, rgt));
            } else if (lft.doubledRight()) {
                return set(R, set(B, lft.left(), lft.root(),
                    lft.right().left()), lft.right().root(),
                    set(B, lft.right().right(), x, rgt));
            } else if (rgt.doubledLeft()) {
                return set(R, set(B, lft, x, rgt.left().left()),
                    rgt.left().root(), set(B, rgt.left().right(),
                    rgt.root(), rgt.right()));
            } else if (rgt.doubledRight()) {
                return set(R, set(B, lft, x, rgt.left()),
                    rgt.root(), rgt.right().paint(B));
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
        set<T> ins(T x) const
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
                    return balance(this->left().ins(x), y, this->right());

                return balance(this->left(), y, this->right().ins(x));
            } else {
                if (x < y)
                    return set(c, this->left().ins(x), y, this->right());

                return set(c, this->left(), y, this->right().ins(x));
            }
        }

        ostream &operator<<(ostream &os)
        {
            os << "[ ";

            forEach([&os](T t) {
                os << t << " ";
            });

            os << "]" << endl;

            return os;
        }
    };
}

#endif
