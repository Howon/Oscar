#ifndef __COLLECTION_H__
#define __COLLECTION_H__

#include <iostream>
#include <atomic>
#include <memory>

using namespace std;

namespace immut {
    enum Color { R, B };
    enum Type {L, S, M};

    template <typename T>
    struct Node {
        Node(Color c, shared_ptr<const Node> const &lft, T val,
            shared_ptr<const Node> const & rgt)
            : _c(c), _lft(lft), _val(val), _rgt(rgt) {}

        Color _c;
        T _val;

        shared_ptr<const Node> _lft;
        shared_ptr<const Node> _rgt;
    };

    template <typename T>
    class collection {
        Type type;

    public:
        collection() {}

        Type getType() const { return type; }

        virtual bool isEmpty() const = 0;

        virtual T front() const = 0;

        virtual bool contains(T x) const = 0;

        template <typename F>
        void forEach(const F f) const {}

        virtual ostream &operator<<(ostream &os) = 0;
    };

    template <typename T>
    class u_collection : collection<T> {
        shared_ptr<const Node<T> > _root;

    public:
        u_collection(Color c, u_collection const &lft, T val,
            u_collection const &rgt)
            : _root(make_shared<const Node<T> >(c, lft._root, val, rgt._root))
        {
            assert(lft.isEmpty() || lft.root() < val);
            assert(rgt.isEmpty() || val < rgt.root());
        }

        u_collection(initializer_list<T> init)
        {
            u_collection t;

            for (T v : init)
                t = t.insert(v);

            _root = t._root;
        }

        virtual void assert1() const = 0;

        virtual bool isEmpty() const { return !_root; }

        T front() const { return _root->val;}

        Color rootColor() const
        {
            assert(!isEmpty());

            return _root->_c;
        }

        template <typename F>
        void forEach(F f)
        {
            static_assert(is_convertible<F, function<void(T)>>::value,
                "ForEach requires a function type void(T)");

            if (!isEmpty()) {
                foreach(f, left());

                f(front());

                foreach(f, right());
            }
        }

        bool contains(T x) const
        {
            if (isEmpty())
                return false;

            T y = front();

            if (x < y)
                return left().contains(x);
            else if (y < x)
                return right().contains(x);
            else
                return true;
        }
    };
}

#endif
