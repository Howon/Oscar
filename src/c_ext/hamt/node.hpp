#ifndef LEAF_HPP
#define LEAF_HPP

#include <string>
#include <vector>
#include <functional>

#include "utils.hpp"

typedef unsigned int u_int;

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
    class internal_node;

    template <typename T>
    class internal_node;

    template <typename T>
    class node {
    protected:
        types type;
        u_int hash;
        u_int mask;
        u_int size;
        string key;
        T val;
        bool can_edit;

        vector<node<T> * > *chdrn;

    public:
        node()
        {
            this->type = EMPTY;
            this->hash = 0;
            this->key = "";
            this->mask = 0;
            this->size = 0;
            this->val = T();
            this->make_mut(false);

            this->chdrn = new vector<node<T> * >();
        }

        node(const node<T>& rhs)
        {
            if (this != &rhs) {
                this->type = rhs.get_type();
                this->hash = rhs.get_hash();
                this->key = rhs.get_key();
                this->mask = rhs.get_mask();
                this->size = rhs.get_size();
                this->val = rhs.get_val();
                this->make_mut(rhs.editable());

                this->chdrn = rhs.get_chdrn();
            }
        }

        node<T> &operator=(const node<T> &rhs)
        {
            if (this == &rhs) {
                return *this;
            }

            this->type = rhs.get_type();
            this->hash = rhs.get_hash();
            this->key = rhs.get_key();
            this->mask = rhs.get_mask();
            this->size = rhs.get_size();
            this->val = rhs.get_val();
            this->make_mut(rhs.editable());

            this->chdrn = rhs.get_chdrn();

            return *this;
        }

        bool operator==(const node<T> &rhs) const {
            if (this == &rhs)
                return true;

            return (this->type == rhs.get_type()) &&
                    (this->hash == rhs.get_hash()) &&
                    (this->key == rhs.get_key()) &&
                    (this->mask == rhs.get_mask()) &&
                    (this->size == rhs.get_size()) &&
                    (this->val == rhs.get_val()) &&
                    (this->editable() == rhs.editable()) &&
                    (this->chdrn == rhs.get_chdrn()) &&
                    (*this->chdrn == *rhs.get_chdrn());
        }

        bool operator!=(const node<T> &rhs) const {
            return !(*this == rhs);
        }
/*
        node<T>(node<T>&& rhs) {
            this->type = rhs.type;
            this->hash = rhs.hash;
            this->key = rhs.key;
            this->mask = rhs.mask;
            this->size = 0;
            this->val = rhs.val;

            this->chdrn = std::move(rhs.chdrn);
        }
*/
        ~node<T>() {}

        types get_type() const { return this->type; }

        u_int get_hash() const { return this->hash; }

        string get_key() const { return this->key; }

        u_int get_mask() const { return this->mask; }

        void set_size(const int size) { this->size = size; }

        int get_size() const { return this->size; }

        T get_val() const { return this->val; }

        bool editable() const { return this->can_edit; }

        void make_mut(bool mode) { this->can_edit = mode; }

        void set_chdrn(vector<node<T> * > *chdrn) { this->chdrn = chdrn; }

        vector<node<T> * > *get_chdrn() const { return this->chdrn; }

        virtual node<T> *modify(const int shift, const u_int hash, u_int *size,
                const string &key, T(*fun)(T))
        {
            return this;
        };

        bool is_leaf() const {
            return get_type() == LEAF || get_type() == COLLISION;
        }
    };

    template <typename T>
    node<T> *merge(bool can_edit, int shift, u_int h_a, node<T> *node_a,
            u_int h_b, node<T> *node_b);

    template <typename T>
    vector<node<T> * > *update_coll(bool can_edit, u_int hash, u_int *size,
            const string &key, T(*fun)(T), vector<node<T> * > *vec);

    template <typename T>
    class empty_node : public node<T> {
    public:
        empty_node() {
            this->type = EMPTY;
            this->hash = 0;
            this->key = "";
        }

        node<T> *modify(const int shift, const u_int hash, u_int *size,
                const string &key, T(*fun)(T));
    };

    template <typename T>
    class leaf_node : public node<T> {
    public:
        leaf_node(bool can_edit, u_int hash, const string &key, const T &val)
        {
            this->type = LEAF;
            this->hash = hash;
            this->key = key;
            this->val = val;
            this->make_mut(can_edit);
        }

        node<T> *modify(const int shift, const u_int hash, u_int *size,
                const string &key, T(*fun)(T));
    };

    template <typename T>
    class collision_node : public node<T> {
    public:
        collision_node(const bool can_edit, const u_int hash,
                vector<node<T> * > *chdrn)
        {
            this->type = COLLISION;
            this->hash = hash;
            this->chdrn = chdrn;
            this->make_mut(can_edit);
        }

        node<T> *modify(const int shift, const u_int hash, u_int *size,
                const string &key, T(*fun)(T));

        node<T> *merge(bool can_edit, int shift, u_int h_a, node<T> *node_a,
            u_int h_b, node<T> *node_b);
    };

    template <typename T>
    class indexed_node : public node<T> {
    public:
        indexed_node(const bool can_edit, const u_int mask,
                vector<node<T> * > *chdrn)
        {
            this->type = INDEX;
            this->mask = mask;
            this->chdrn = chdrn;
            this->make_mut(can_edit);
        }

        node<T> *modify(const int shift, const u_int hash, u_int *size,
                const string &key, T(*fun)(T));

        internal_node<T> *expand(const u_int part, const u_int bit_map,
            const hamt::node<T> child);
    };

    template <typename T>
    class internal_node : public node<T> {
    public:
        internal_node(const bool can_edit, const int size,
                vector<hamt::node<T> * > *chdrn)
        {
            this->type = INTERNAL;
            this->size = size;
            this->chdrn = chdrn;
            this->make_mut(can_edit);
        }

        indexed_node<T> *pack(const int count, const int removed) const
        {
            auto chdrn = new vector<node<T> *>(count - 1);

            int g = 0;
            int bit_map = 0;

            for (int i = 0, len = this->chdrn->size(); i < len; i++) {
                if (i != removed) {
                    auto elem = (*this->chdrn)[i];

                    if (elem->get_type() != EMPTY) {
                        (*chdrn)[g++] = elem;
                        bit_map |= 1 << i;
                    }
                }
            }

            return new indexed_node<T>(this->editable(), bit_map, chdrn);
        }

        node<T> *modify(const int shift, const u_int hash, u_int *size,
                const string &key, T(*fun)(T));
    };
};

using namespace hamt;

template <typename T>
node<T> *empty_node<T>::modify(const int shift, const u_int hash, u_int *size,
        const string &key, T(*fun)(T))
{
    T v = fun(T());

    if (!v)
        return new empty_node<T>();;

    *size += 1;

    return new leaf_node<T>(this->editable(), hash, key, v);
}

template<typename T>
node<T> *leaf_node<T>::modify(const int shift, const u_int hash, u_int *size,
        const string &key, T(*fun)(T))
{
    T val;

    if (key == this->key) {
        val = fun(this->get_val());

        if (val == this->get_val())
            return this;

        if (!val) {
            *size -= 1;

            return new empty_node<T>();;
        }

        if (this->editable()) {
            this->val = val;

            return this;
        }

        return new leaf_node<T>(this->editable(), hash, key, val);
    }

    val = fun(T());

    if (!val)
        return this;

    *size += 1;

    return new empty_node<T>();;//merge<T>(this->editable(), shift, this->hash, *this,
            //hash, leaf_node<T>(hash, key, val));
}

template <typename T>
node<T> *collision_node<T>::modify(const int shift, const u_int hash, u_int *size,
        const string &key, T(*fun)(T))
{
    if (hash == this->hash) {
        auto vec = update_coll(this->editable(), hash, size, key, fun,
                this->get_chdrn());

        if (vec == this->get_chdrn())
            return this;

        if (!vec->empty())
            return new collision_node<T>(this->editable(), hash, vec);
        else
            return (*vec)[0];
    }

    T val = fun(T());

    if (!val)
        return this;

    *size += 1;

    return new leaf_node<T>(this->editable(), hash, key, val);
        //merge<T>(this->editable(), shift, this->hash, *this,
         //   hash, leaf_node<T>(hash, key, val));
}

template <typename T>
node<T> *indexed_node<T>::modify(const int shift, const u_int hash, u_int *size,
        const string &key, T(*fun)(T))
{
    u_int hp = hash_part(shift, hash);
    u_int bit = to_bit_map(hp);
    u_int index = from_bit_map(this->mask, bit);

    bool child_exists = hp & bit;

    auto chdrn = this->get_chdrn();

    auto curr = child_exists ? (*chdrn)[index] : new empty_node<T>();;
    auto child = curr->modify(shift + SIZE, hash, size, key, fun);

    if (curr == child)
        return this;

    if (child_exists && child->get_type() == EMPTY) {
        u_int bit_map = this->mask & ~bit;

        if (bit_map == 0)
            return new empty_node<T>();;

        if (chdrn->size() <= 2 && (*chdrn)[index ^ 1]->get_type() == LEAF)
            return (*chdrn)[index ^ 1];

        return new indexed_node<T>(this->editable(), bit_map,
                vec_rm<node<T> *>(this->editable(), chdrn, index));
    }

    if (!child_exists && child->get_type() != EMPTY) {
        if (chdrn->size() >= MAX_INDEX_NODE)
            return this;//expand(hp, child, this->mask);

        return new indexed_node<T>(this->editable(), this->mask | bit,
                    vec_put<node<T> *>(this->editable(), chdrn, index, child));
    }

    return new indexed_node<T>(this->editable(), this->mask,
                vec_upd<node<T> *>(this->editable(), chdrn, index, child));
}

template <typename T>
node<T> *internal_node<T>::modify(const int shift, const u_int hash, u_int *size,
        const string &key, T(*fun)(T))
{
    int count = this->size;
    u_int hp = hash_part(shift, hash);

    auto chdrn = this->get_chdrn();
    auto child = hp < chdrn->size() ? (*chdrn)[hp] : new empty_node<T>();;
    auto new_child = child->modify(shift + SIZE, hash, size, key, fun);
    auto new_chdrn = vec_upd<node<T> *>(this->editable(), chdrn, hp, new_child);

    if (child == new_child)
        return new internal_node<T>(this->editable(), this->size + 1, new_chdrn);

    if (child->get_type() == EMPTY && new_child->get_type() == EMPTY) {
        count++;
    } else if (child->get_type() != EMPTY && new_child->get_type() != EMPTY) {
        if (--count <= MIN_ARRAY_NODE)
            return pack(count, hp);

        new_chdrn =
            vec_upd<node<T> *>(this->editable(), chdrn, hp, new empty_node<T>());
    }

    if (this->editable()) {
        this->set_size(count);
        this->set_chdrn(new_chdrn);

        return this;
    }

    return new internal_node<T>(this->editable(), this->size, new_chdrn);
}

template <typename T>
node<T> *hamt::merge(bool can_edit, int shift, u_int h_a, node<T> *node_a,
        u_int h_b, node<T> *node_b)
{
    if (h_a == h_b)
        return new collision_node<T>(can_edit, h_a,
                new vector<node<T> * >({node_a, node_b}));

    u_int hp_a = hash_part(shift, h_a);
    u_int hp_b = hash_part(shift, h_b);

    u_int bit_map_merge = to_bit_map(hp_a) | to_bit_map(hp_b);

    auto nodes = new vector<node<T> *>{node_a, node_b};

    if (hp_a == hp_b) {
        node<T> * merge =
                merge(can_edit, shift + SIZE, h_a, node_a, h_b, node_b);

        nodes->clear();
        nodes->push_back(merge);
    } else {
        if (hp_a > hp_b) {
            nodes->clear();
            nodes->push_back(node_b);
            nodes->push_back(node_a);
        }
    }

    return new indexed_node<T>(can_edit, bit_map_merge, nodes);
}

template <typename T>
vector<node<T> *> *hamt::update_coll(bool can_edit, u_int hash, u_int *size,
        const string &key, T(*fun)(T), vector<node<T> *> *vec)
{
    int length = vec->size();

    T val;

    for (int i = 0; i < length; i++) {
        auto child = (*vec)[i];

        if (child->get_key() == key) {
            T c_val = child->get_val();
            val = fun(c_val);

            if (c_val == val)
                return vec;

            if (!val) {
                *size -= 1;

                return vec_rm<node<T> * >(can_edit, vec, i);
            }

            return vec_upd<node<T> * >(can_edit, vec, i,
                        new leaf_node<T>(can_edit, hash, key, val));
        }
    }

    val = fun(T());

    if (!val)
        return vec;

    *size += 1;

    return vec_upd<node<T> * >(can_edit, vec, length,
                new leaf_node<T>(can_edit, hash, key, val));
}
/*
template <typename T>
internal_node<T> *indexed_node<T>::expand(const u_int part, const u_int bit_map,
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

    return new internal_node<T>(count + 1, new_vec);
}

*/
#endif
