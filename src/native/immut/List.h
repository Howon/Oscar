#ifndef __LIST_H__
#define __LIST_H__

#include <iostream>
#include <cassert>
#include <memory>
#include <functional>
#include <initializer_list>
#include <iterator>

#include "collection.h"

using namespace std;

namespace immut {
  template <typename T>
  class list : public collection<T> {
      Type type = L;

      struct Item {
          Item(T v, shared_ptr<const Item> tail)
              : _val(v), _next(move(tail))
          {}

          explicit Item(T v) : _val(v) {}

          T _val;

          shared_ptr<const Item> _next;
      };

      friend Item;

      explicit list(shared_ptr<const Item> items) : _head(move(items)) {}

      shared_ptr<const Item> _head;

  public:
      list(T v, list const &tail) : _head(make_shared<Item>(v, tail._head)) {}

      list(list const &head, T v) : _head(make_shared<Item>(head._head, v)) {}

      explicit list(T v) : _head(make_shared<Item>(v)) {}

      list(initializer_list<T> init)
      {
          for (auto it = rbegin(init); it != rend(init); ++it) {
              _head = make_shared<Item>(*it, _head);
          }
      }

      virtual bool isEmpty() const { return !_head; } // conversion to bool

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

      list push_front(T v) const
      {
          return list(v, *this);
      }

      list push_back(T v) const
      {
          return list(*this, v);
      }

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
        cout << " hererr" << endl;
          Item const * it = _head.get();
    cout << it->_val << endl;
          while (it != nullptr) {
              f(it->_val);
              it = it->_next.get();
          }
      }

      virtual ostream &operator<<(ostream &os)
      {
          os << "[ ";

          forEach([&os](T v) {
              os << v << " ";
          });

          os << "]";

          return os;
      }
  };
}

#endif
