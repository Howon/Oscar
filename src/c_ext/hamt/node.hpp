#ifndef LEAF_HPP
#define LEAF_HPP

#include <string>
#include <vector>
#include <functional>

#include "utils.hpp"

typedef unsigned int u_int;

template <typename T>
class leaf_node;

template <typename T>
class collision_node;

template <typename T>
class indexed_node;

template <typename T>
class internal_node;


using namespace std;

enum types {
    EMPTY,
    LEAF,
    COLLISION,
    INDEX,
    INTERNAL
};

namespace hamt {

    template <typename T>
    class node
    {
    protected:
        types type;
        u_int hash;
        string key;
        T val;
        u_int mask;
        vector<node> children;

    public:
        node() {
            this->type = EMPTY;
            this->hash = 0;
            this->key = "";
            this->val = NULL;
            this->mask = 0;
            this->children = NULL;
        }

        node<T> &operator=(const node<T> &rhs)
        {
            if (this == &rhs)
                return *this;

            this->type = rhs.type;
            this->hash = rhs.hash;
            this->key = rhs.key;
            this->val = rhs.val;
            this->mask = rhs.mask;
            this->vector = rhs.vector;

            return *this;
        }

        bool operator==(const node<T> &rhs) { return this == &rhs; }

        types get_type() { return type; }

        virtual node &modify(const int shift, T(*fun)(T),
            const u_int hash, const string key, int *size) = 0;

        bool is_leaf() {
            return get_type() == LEAF || get_type() == COLLISION;
        }

        vector<node> get_children() { return this->children; }
    };

    template <typename T>
    class empty_node : node<T>
    {
        node<T> &modify(const int shift, T(*fun)(T),
            const u_int hash, const string key, int *size)
        {
            T v = fun(NULL);

            if (!v)
                return empty_node();

            *size += 1;

            return leaf_node<T>(hash, key, v);
        }
    };

    template <typename T>
    node<T> __merge(const int shift, const u_int h_a,
            const node<T> node_a, const u_int h_b, const node<T> node_b)
    {
        if (h_a == h_b)
            return collision_node<T>(h_a, vector<node<T>> {node_a, node_b});

        u_int hp_a = hash_part(shift, h_a);
        u_int hp_b = hash_part(shift, h_b);

        u_int bit_map_merge = to_bit_map(hp_a) | to_bit_map(hp_b);

        vector<node<T> > nodes;

        if (hp_a == hp_b)
            nodes = __merge(shift + SIZE, h_a, node_a, h_b, node_b);
        else {
            if (hp_a > hp_b)
                nodes = vector<node<T> > {node_b, node_a};

            nodes = vector<node<T> > {node_a, node_b};
        }

        return indexed_node<T>(bit_map_merge, nodes);
    }

    template <typename T>
    vector<node<T> > __update_coll(const u_int hash, const string key,
            T(*fun)(T), vector<node<T> > vec, int *size)
    {
        int length = vec.size();

        T val;

        for (int i = 0; i < length; i++) {
            auto child = vec[i];

            if (child.key == key) {
                T c_val = child.value;
                val = f(c_val);

                if (c_val == val)
                    return vec;

                if (!val) {
                    *size -= 1;

                    return vec_rm(i, vec);
                }

                return vec_upd(i, leaf_node<T>(hash, key, val), vec);
            }
        }

        val = fun(NULL);

        if (!val)
            return vec;

        *size += 1;

        return vec_upd(length, leaf_node<T>(hash, key, val), vec);
    }

    template <typename T>
    class leaf_node : node<T>
    {
    private:
        u_int hash;
        string key;
        T val;

    public:
        leaf_node(const u_int hash, const string key, const T val)
        {
            this->type = LEAF;
            this->hash = hash;
            this->key = key;
            this->val = val;
        }

        node<T> &modify(const int shift, T(*fun)(T),
            const u_int hash, const string key, int *size)
        {
            T val;

            if (key == this->key)
            {
                val = f(this->value);

                if (val == this->value)
                    return this;

                if (!val) {
                    *size -= 1;
                    return empty_node<T>();
                }

                return leaf_node<T>(hash, key, val);
            }

            val = fun(NULL);

            if (!val)
                return this;

            *size += 1;

            return __merge(shift, this->hash, this,
                    hash, leaf_node<T>(hash, key, val));
        }

    };

    template <typename T>
    class collision_node : node<T>
    {
    private:
        u_int hash;
        vector<leaf_node<T> > children;

    public:
        collision_node(const u_int hash, const vector<leaf_node<T> > children)
        {
            this->type = COLLISION;
            this->hash = hash;
            this->children = children;
        }

        node<T> &modify(const int shift, T(*fun)(T),
            const u_int hash, const string key, int *size)
        {
            if (hash == this->hash) {
                vector<node<T> > vec =
                    __update_coll(hash, key, fun, this->children, size);

                if (vec == this->children)
                    return this;

                return !vec.empty() ?
                    collision_node<T>(hash, vec) : vec[0];
            }

            T val = fun(NULL);

            if (!val)
                return this;

            *size += 1;

            return __merge(shift, this->hash, this,
                    hash, leaf_node<T>(hash, key, val));
        }
    };

    template <typename T>
    class indexed_node : hamt::node<T>
    {
    private:
        u_int mask;
        vector<hamt::node<T> > children;

    public:
        indexed_node(const u_int mask, const vector<hamt::node<T> > children)
        {
            this->type = INDEX;
            this->mask = mask;
            this->children = children;
        }

        internal_node<T> expand(const u_int part, const u_int bit_map,
            const hamt::node<T> child)
        {
            auto new_vec = vector<hamt::node<T> >();

            empty_node<T> empty = empty_node<T>();

            u_int bit = bit_map;
            int count = 0;

            for (u_int i = 0; i < bit; i++) {
                if (bit & 1)
                    new_vec.push(this->sub_nodes[count++]);

                new_vec.push(empty);

                bit >>= 1;
            }

            new_vec[part] = child;

            return internal_node<T>(count + 1, new_vec);
        }

        node<T> &modify(const int shift, T(*fun)(T),
                const u_int hash, const string key, int *size)
        {
            u_int hp = hash_part(shift, hash);
            u_int bit = to_bit_map(hp);
            u_int index = from_bit_map(this->mask, bit);

            bool child_exists = hp & bit;

            auto chdrn = this->children;

            auto curr = child_exists ? chdrn[index] : empty_node<T>();
            auto child = curr.modify(shift + SIZE, fun, hash, key, size);

            if (curr == child)
                return this;

            if (child_exists && child.type == EMPTY) {
                u_int bit_map = this->mask & ~bit;

                if (bit_map == 0)
                    return empty_node<T>();

                if (chdrn.size() <= 2 && chdrn[index ^ 1].type == LEAF)
                    return chdrn[index ^ 1];

                return indexed_node<T>(bit_map, vec_rm(index, chdrn));
            }

            if (!child_exists && child.type != EMPTY) {
                if (chdrn.size() >= MAX_INDEX_NODE)
                    return expand(hp, child, this->mask, chdrn);

                return indexed_node<T>(this->mask | bit,
                        vec_put(index, child, chdrn));
            }

            return indexed_node<T>(this->mask,
                    vec_upd(index, child, chdrn));
        }

    };

    template <typename T>
    class internal_node : hamt::node<T>
    {
    private:
        int size;
        vector<hamt::node<T> > children;
    public:
        internal_node(const int size, const vector<hamt::node<T> > children)
        {
            this->type = INTERNAL;
            this->size = size;
            this->children = children;
        }

        indexed_node<T> pack(const int count, const int removed)
        {
            auto children = vector<hamt::node<T> >(count - 1);

            int g = 0;
            int bit_map = 0;

            for (int i = 0, len = this->children.size(); i < len; i++) {
                if (i != removed) {
                    hamt::node<T> elem = this->children[i];

                    if (elem.type != EMPTY) {
                        children[g++] = elem;
                        bit_map |= 1 << i;
                    }
                }
            }

            return indexed_node<T>(bit_map, children);
        }

        node<T> &modify(const int shift, T(*fun)(T),
                const u_int hash, const string key, int *size)
        {
            u_int hp = hash_part(shift, hash);

            auto chdrn = this->children;
            auto child = hp < chdrn.size() ? chdrn[hp] : empty_node<T>();
            auto new_child = child.modify(shift + SIZE, fun, hash, key, size);

            if (child == new_child) {
                return internal_node<T>(this->size + 1,
                        vec_upd(hp, new_child, chdrn));
            }

            if (child.type != EMPTY && new_child.type != EMPTY) {
                if (this->size - 1 <= MIN_ARRAY_NODE)
                    return pack(hp, new_child, chdrn);

                return internal_node<T>(this->size - 1,
                        vec_upd(hp, empty_node<T>(), chdrn));
            }

            return internal_node<T>(this->size, vec_upd(hp, new_child, chdrn));
        }

    };
};

#endif
