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
    enum Color { R, B };
    enum Type {L, S, M};

    template <typename T>
    class collection {
        Type type;

    public:
        Type getType() const { return type; }

        virtual bool isEmpty() const = 0;

        virtual bool contains(T x) const = 0;
    };
}

#endif
