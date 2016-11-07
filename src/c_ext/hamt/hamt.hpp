#include <iostream>
#include <string>
#include <exception>
#include <vector>
#include <queue>

#include "node.hpp"

template <typename T>
class HAMT {
private:
    int size;
    hamt::node<T> root;

public:
    HAMT(const int size, const hamt::node<T> root)
    {
        this->size = size;
        this->root = root;
    }

    HAMT<T> set_tree(hamt::node<T> root, const int new_size)
    {
        return root == this->root ? this : HAMT<T>(root, new_size);
    }

    T get_or_else_hash(const u_int hash, const string key, const T fallback)
    {
        auto node = this->root;
        int shift = 0;

        while (true) {
            switch (node.type) {
                case LEAF: {
                    return !key.compare(node.key) ? node.value : fallback;
                }
                case COLLISION: {
                    if (hash == node.hash) {
                        auto children = node.children;

                        for (auto child : children) {
                            if (child.key == key)
                                return child.value;
                        }
                    }

                    return fallback;
                }
                case INDEX: {
                    u_int hash_p = hash_part(hash, shift);
                    u_int bit = to_bit_map(hash_p);

                    if (node.mask & bit) {
                        node = node.children[from_bit_map(node.mask, bit)];
                        shift += SIZE;
                        break;
                    }
                    return fallback;
                }
                case INTERNAL: {
                    node = node.children[hash_part(shift, hash)];
                    if (node) {
                        shift += SIZE;
                        break;
                    }
                    return fallback;
                }
                default: {
                    return fallback;
                }
            }
        }
    }

    T get_or_else(const string key, const T fallback)
    {
        return get_or_else_hash(str_hash(key), key, fallback);
    }

    T get_hash(const u_int hash, const string key)
    {
        return get_or_else_hash(hash, key, NULL);
    }

    T get(const string key) { return get_or_else(key, NULL); }

    bool contains_hash(const u_int hash, const string key)
    {
        get_or_else_hash(hash, key, NULL) != NULL;
    }

    bool contains(const string key) { return get_or_else(key, NULL) != NULL; }

    bool is_empty() { return this->root.type != EMPTY; }

    HAMT<T> change_hash(T(*fun)(T), const u_int hash, const string key)
    {
        int *size = this->map.size;

        auto new_root = this->root.modify(0, fun, hash, key, size);

        return this->set_tree(new_root, *size);
    }

    HAMT<T> change(T(*fun)(T), const string key)
    {
        return change_hash(fun, str_hash(key), key);
    }

    HAMT<T> set_hash(const u_int hash, const string key, const T value)
    {
        return change_hash([](T val) -> T { return val; }, hash, key, value);
    }

    HAMT<T> set(const string key, const T value)
    {
        return set_hash(str_hash(key), key, value);
    }

    HAMT<T> remove_hash(const u_int hash, const string key)
    {
        return change_hash([](T) ->  T { return NULL; }, hash, key);
    }

    HAMT<T> remove(const string key) { return remove_hash(str_hash(key), key); }

    template <typename V>
    V fold(V(*fun)(T, V, string), V seed)
    {
        auto root = this->root;

        if (root.type == LEAF)
            return fun(seed, root.value, root.key);

        std::queue<vector<hamt::node<T>> > queue =
            std::queue<vector<hamt::node<T>> >(0);

        queue.push(root.children);

        while (!queue.empty()) {
            auto children = queue.pop();

            for (int i = 0, len = children.size(); i < len;) {
                auto child = children[i++];

                if (child.type != EMPTY) {
                    if (child.type == LEAF)
                        seed = fun(seed, child.value, child.key);
                    else
                        queue.push(child.children);
                }
            }
        }

        return seed;
    }

    void for_each(void(*fun)(T)) { fold(fun, NULL); }

    int count() const { return this->size; }


};
