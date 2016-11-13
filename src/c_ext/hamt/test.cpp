#include <iostream>
#include "hamt.hpp"

#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#define NUM_CHILDREN 32

using namespace std;

TEST_CASE("Empty node equality", "[equality]")
{
    auto empty = hamt::empty_node<int>();
    auto empty1 = hamt::empty_node<int>();
    auto empty2 = empty;

    REQUIRE(empty == empty1);
    REQUIRE(empty2 == empty1);
    REQUIRE(empty == empty2);
    REQUIRE(empty == empty);
}

TEST_CASE("Leaf node equality", "[equality]")
{
    string key = "node key";
    u_int hash = str_hash(key);
    int val = 4000;

    auto leaf = hamt::leaf_node<int>(false, hash, key, val);
    auto same_leaf = hamt::leaf_node<int>(false, hash, key, val);
    auto same_ref = leaf;

    auto diff_hash = hamt::leaf_node<int>(false, hash + 5, key, val);
    auto diff_key = hamt::leaf_node<int>(false, hash, key + key, val);
    auto diff_val = hamt::leaf_node<int>(false, hash, key, val - 5);

    REQUIRE(leaf == same_leaf);
    REQUIRE(same_ref == same_leaf);
    REQUIRE(leaf == same_ref);
    REQUIRE(leaf == leaf);

    REQUIRE(leaf != diff_hash);
    REQUIRE(leaf != diff_val);
    REQUIRE(leaf != diff_key);
}

TEST_CASE("Collision node equality", "[equality]")
{
    string key = "node key";
    u_int hash = str_hash(key);
    int val = 4000;

    auto node = hamt::leaf_node<int>(false, hash, key, val);
    auto node_diff = hamt::leaf_node<int>(false, hash, key, val + 4);

    auto children = vector<hamt::node<int> * >(NUM_CHILDREN, &node);

    auto diff_children = vector<hamt::node<int> * >(NUM_CHILDREN - 1, &node);

    auto diff_children_2 =
        vector<hamt::node<int> * >(NUM_CHILDREN, new empty_node<int>());

    auto diff_children_3 =
        vector<hamt::node<int> * >(NUM_CHILDREN, &node_diff);

    auto collision = hamt::collision_node<int>(false, hash, &children);
    auto same_coll = hamt::collision_node<int>(false, hash, &children);
    auto same_ref = collision;

    auto diff_hash = hamt::collision_node<int>(false, hash + 5, &children);
    auto diff_chdrn = hamt::collision_node<int>(false, hash, &diff_children);
    auto diff_chdrn_2 = hamt::collision_node<int>(false, hash, &diff_children_2);
    auto diff_chdrn_3 = hamt::collision_node<int>(false, hash, &diff_children_3);

    REQUIRE(collision == same_coll);
    REQUIRE(same_ref == same_coll);
    REQUIRE(collision == same_ref);
    REQUIRE(collision == collision);

    REQUIRE(collision != diff_hash);
    REQUIRE(collision != diff_chdrn);
    REQUIRE(collision != diff_chdrn_2);
    REQUIRE(collision != diff_chdrn_3);
    REQUIRE(diff_chdrn != diff_chdrn_2);
}

TEST_CASE("Indexed node equality", "[equality]")
{
    string key = "node key";
    u_int mask= 40404;
    int val = 4000;

    auto node = hamt::leaf_node<int>(false, mask, key, val);
    auto node_diff = hamt::leaf_node<int>(false, mask, key, val + 4);

    auto children = vector<hamt::node<int> * >(NUM_CHILDREN, &node);

    auto diff_children =
        vector<hamt::node<int> * >(NUM_CHILDREN - 1, &node);

    auto diff_children_2 =
        vector<hamt::node<int> * >(NUM_CHILDREN, new empty_node<int>());

    auto diff_children_3 =
        vector<hamt::node<int> * >(NUM_CHILDREN, &node_diff);

    auto indexed = hamt::indexed_node<int>(false, mask, &children);
    auto same_index = hamt::indexed_node<int>(false, mask, &children);
    auto same_ref = indexed;

    auto diff_hash = hamt::indexed_node<int>(false, mask + 5, &children);
    auto diff_chdrn = hamt::indexed_node<int>(false, mask, &diff_children);
    auto diff_chdrn_2 = hamt::indexed_node<int>(false, mask, &diff_children_2);
    auto diff_chdrn_3 = hamt::indexed_node<int>(false, mask, &diff_children_3);

    REQUIRE(indexed == same_index);
    REQUIRE(same_ref == same_index);
    REQUIRE(indexed == same_ref);
    REQUIRE(indexed == indexed);

    REQUIRE(indexed != diff_hash);
    REQUIRE(indexed != diff_chdrn);
    REQUIRE(indexed != diff_chdrn_2);
    REQUIRE(indexed != diff_chdrn_3);
    REQUIRE(diff_chdrn != diff_chdrn_2);
}

TEST_CASE("Internal node equality", "[equality]") {
    string key = "node key";
    u_int mask= 50;
    int val = 4000;

    auto node = hamt::leaf_node<int>(false, mask, key, val);
    auto node_diff = hamt::leaf_node<int>(false, mask, key, val + 4);

    auto children = vector<hamt::node<int> * >(NUM_CHILDREN, &node);

    auto diff_children =
        vector<hamt::node<int> * >(NUM_CHILDREN - 1, &node);

    auto diff_children_2 =
        vector<hamt::node<int> * >(NUM_CHILDREN, new empty_node<int>());

    auto diff_children_3 =
        vector<hamt::node<int> * >(NUM_CHILDREN, &node_diff);

    auto internal = hamt::internal_node<int>(false, mask, &children);
    auto same_internal = hamt::internal_node<int>(false, mask, &children);
    auto same_ref = internal;

    auto diff_hash = hamt::internal_node<int>(false, mask + 5, &children);
    auto diff_chdrn = hamt::internal_node<int>(false, mask, &diff_children);
    auto diff_chdrn_2 = hamt::internal_node<int>(false, mask, &diff_children_2);
    auto diff_chdrn_3 = hamt::internal_node<int>(false, mask, &diff_children_3);

    REQUIRE(internal == same_internal);
    REQUIRE(same_ref == same_internal);
    REQUIRE(internal == same_ref);
    REQUIRE(internal == internal);

    REQUIRE(internal != diff_hash);
    REQUIRE(internal != diff_chdrn);
    REQUIRE(internal != diff_chdrn_2);
    REQUIRE(internal != diff_chdrn_3);
    REQUIRE(diff_chdrn != diff_chdrn_2);
}

TEST_CASE("Node type equality", "[equality]")
{
    string key = "node key";
    u_int mask= 50;
    u_int hash = str_hash(key);
    int val = 4000;

    auto leaf = hamt::leaf_node<int>(false, hash, key, val);
    auto node_diff = hamt::leaf_node<int>(false, hash, key, val + 4);

    auto children = vector<hamt::node<int> * >(NUM_CHILDREN, &leaf);

    auto empty = hamt::empty_node<int>();
    auto collision = hamt::collision_node<int>(false, hash, &children);
    auto indexed = hamt::indexed_node<int>(false, mask, &children);
    auto internal = hamt::internal_node<int>(false, mask, &children);

    REQUIRE(empty != leaf);
    REQUIRE(leaf != collision);
    REQUIRE(collision != indexed);
    REQUIRE(indexed!= internal);
}

TEST_CASE("Merge test", "[functionality]")
{
    int shift = 0;
    string key_a = "node A";   string key_b = "node B";

    u_int hash_a = str_hash(key_a);
    u_int hash_b = str_hash(key_b);

    auto node_a = leaf_node<int>(false, hash_a, key_a, 5);
    auto node_b = leaf_node<int>(false, hash_b, key_b, 10);

    auto merged_coll = hamt::__merge(false, shift, hash_a, &node_a, hash_b, &node_b);

    cout << merged_coll->get_type() << endl;
    cout << merged_coll->get_hash() << endl;
    cout << merged_coll->get_key() << endl;
    cout << merged_coll->get_mask() << endl;
    cout << merged_coll->get_val() << endl;
    cout << merged_coll->get_size() << endl;
}
