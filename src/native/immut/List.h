#ifndef __LIST_H__
#define __LIST_H__

#include "collection.h"

using namespace std;

namespace immut {
    template <typename T>
    class list : public collection<T> {
        Type type = L;

        struct Item {
            Item(T v, shared_ptr<const Item> tail) :
                _hash(rand()), _val(v), _next(move(tail)) {}

            explicit Item(T v) : _val(v) {}

            int _hash;
            T _val;

            shared_ptr<const Item> _next;
        };

        friend Item;

        shared_ptr<const Item> _head;

        explicit list(shared_ptr<const Item> items) : _head(move(items)) {}

    public:
        list() {}

        list(T v, list const &tail) :
            _head(make_shared<Item>(v, tail._head)) {}

        explicit list(T v) : _head(make_shared<Item>(v)) {}

        list(initializer_list<T> init)
        {
            for (auto it = rbegin(init); it != rend(init); ++it) {
                _head = make_shared<Item>(*it, _head);
            }
        }

        shared_ptr<const Item> get_root() const { return _head; }

        virtual bool isEmpty() const { return !_head; }

        virtual T front() const
        {
            assert(!isEmpty());

            return _head->_val;
        }

        list pop_front() const
        {
            assert(!isEmpty());

            return list(_head->_next);
        }

        list push_front(T v) const { return list(v, *this); }

        list take(int n)
        {
            if (n <= 0 || isEmpty())
              return list();

            return pop_front().take(n - 1).push_front(front());
        }

        list insertedAt(int i, T v) const
        {
            if (i == 0)
                return push_front(v);
            else {
                assert(!isEmpty());

                return list<T>(front(), pop_front().insertedAt(i - 1, v));
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

        list removed(T v) const
        {
            if (isEmpty())
              return list();

            if (v == front())
                return pop_front();

            return list(front(), pop_front().removed(v));
        }

        bool contains(T v) const
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
            Item const *it1 = get_root().get();
            Item const *it2 = rhs.get_root().get();

            while (!(it1 == nullptr || it2 == nullptr)) {
                if (it1->_val != it2->_val)
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

            t.forEach([&os](T v) {
                os << v << " ";
            });

            os << "]";

            return os;
        }
    };
}

#endif
