#ifndef UTILS_HPP
#define UTILS_HPP

#include <iostream>
#include <string>
#include <exception>
#include <vector>

#define SIZE 5
#define BUCKET_SIZE 2 << SIZE
#define MASK (BUCKET_SIZE - 1)
#define MAX_INDEX_NODE BUCKET_SIZE / 2
#define MIN_ARRAY_NODE BUCKET_SIZE / 4

typedef unsigned int u_int;

static inline u_int str_hash(std::string str)
{
    u_int hash = 0;

    for (int i = 0; i < str.length(); i++) {
        hash = ((hash << 5) + hash) + str[i]; /* hash * 33 + c */
    }

    return hash;
}

/**
 * O(1) function to calculate the hamming weight of given unsigned int
 * @param  x  unsigned integer
 * @return Hamming weight
 */
static inline int pop_count(const u_int x)
{
    int y = x;

    y -= y >> 1 & 0x55555555;
    y = (y & 0x33333333) + (y >> 2 & 0x33333333);
    y = y + (y >> 4) & 0x0f0f0f0f;
    y += y >> 8;
    y += y >> 16;

    return y & 0x7f;
}

/**
 * Gets the part of a hash we are interested in at a given level.
 * @param level  Level/depth of the trie
 * @param hash  Hash value.
 * @return Hash partition
 */
static inline u_int hash_part(const u_int hash, const int level)
{
    if (!(level % SIZE))
        throw std::invalid_argument("INVALID SHIFT AMOUNT");

    return (hash >> level) & (SIZE - 1);
}

/**
 * Converts a hash partition to a 32bit bitmap
 * with the bit at index partition set.
 * @param hash_part  Hash partition bit
 * @return Bitmap generated from given hash partition
 */
static inline u_int to_bit_map(const u_int hash_part)
{
    return 1 << hash_part;
}

/**
 * Converts a hash partition to an index in a
 * dense child array with a given bitmap
 * @param bit_map [description]
 * @param bit     [description]
 * @return
 */
static inline int from_bit_map(const u_int bit_map, const u_int bit)
{
    return pop_count(bit_map & bit - 1);
}

template <typename T>
std::vector<T> vec_rm(std::vector<T> vec, const int index)
{
    if (index > vec.size)
        throw std::invalid_argument("INVALID INDEX");

    std::vector<T> new_vec = new std::vector<T>(vec);
    new_vec.erase(new_vec.begin() + index);
    return new_vec;
}

template <typename T>
std::vector<T> vec_put(std::vector<T> vec, const int index, const T val)
{
    if (index > vec.size)
        throw std::invalid_argument("INVALID INDEX");

    std::vector<T> new_vec = new std::vector<T>(vec);

    new_vec.insert(new_vec.begin() + index, val);

    return new_vec;
}

template <typename T>
std::vector<T> vec_upd(std::vector<T> vec, const int index, const T val)
{
    std::vector<T> new_vec = new std::vector<T>(vec);

    if (index < new_vec.size())
        new_vec[index] = val;
    else
        new_vec.push(val);

    return new_vec;
}

#endif
