#include <vector>
#include "gtest/gtest.h"
#include "feniks/array.hpp"

#define TEST_ARRAY_LOOP(type, val, array, test) \
    for (type val : array) {                    \
        test;                                   \
    }

#define TEST_ARRAY_LOOP_WITH_COUNTER(type, val, array, counter, test) \
    counter = 0;                                                      \
    for (type val : array) {                                          \
        test;                                                         \
        ++counter;                                                    \
    }

#define TEST_ARRAY_REVERSE_LOOP_WITH_COUNTER(val, array, counter, test)      \
    counter = 0;                                                             \
    for (auto val = array.rbegin(); val != array.rend(); ++val, ++counter) { \
        test;                                                                \
    }

#define TEST_ARRAY_INDEX_LOOP(val, array, axis, test)     \
    for (size_t val = 0; val < array.size(axis); ++val) { \
        test;                                             \
    }

using namespace feniks;

TEST(ArrayTest, ArraySize) {
    array<int, 1> a(5);
    EXPECT_EQ(a.size(), 5);
    EXPECT_EQ(a.full_size(), 5);

    array<int, 1> b(10);
    EXPECT_EQ(b.size(), 10);
    EXPECT_EQ(b.full_size(), 10);

    array<int, 2> c(4, 2);
    EXPECT_EQ(c.size(), 4);
    EXPECT_EQ(c.size(0), 4);
    EXPECT_EQ(c.size(1), 2);
    EXPECT_EQ(c.full_size(), 8);

    array<int, 10> d(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    EXPECT_EQ(d.size(), 1);
    EXPECT_EQ(d.size(0), 1);
    EXPECT_EQ(d.size(1), 2);
    EXPECT_EQ(d.size(2), 3);
    EXPECT_EQ(d.size(3), 4);
    EXPECT_EQ(d.size(4), 5);
    EXPECT_EQ(d.size(5), 6);
    EXPECT_EQ(d.size(6), 7);
    EXPECT_EQ(d.size(7), 8);
    EXPECT_EQ(d.size(8), 9);
    EXPECT_EQ(d.size(9), 10);
    EXPECT_EQ(d.full_size(), 3628800);
}

TEST(ArrayTest, ArrayIndexing) {
    array<int, 1> a{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a[i], i));
    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a[{i}], i));
    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a.at(i), i));
    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a.at({i}), i));

    array<int, 2> b{ { 0, 1 }, { 2, 3 }, { 4, 5 }, { 6, 7 }, { 8, 9 } };

    TEST_ARRAY_INDEX_LOOP(i, b, 0, TEST_ARRAY_INDEX_LOOP(j, b, 1, EXPECT_EQ((b[{i, j}]), i * 2 + j)));
    TEST_ARRAY_INDEX_LOOP(i, b, 0, TEST_ARRAY_INDEX_LOOP(j, b, 1, EXPECT_EQ(b[i][j], i * 2 + j)));
    TEST_ARRAY_INDEX_LOOP(i, b, 0, TEST_ARRAY_INDEX_LOOP(j, b, 1, EXPECT_EQ(b.at(i, j), i * 2 + j)));
    TEST_ARRAY_INDEX_LOOP(i, b, 0, TEST_ARRAY_INDEX_LOOP(j, b, 1, EXPECT_EQ(b.at({i, j}), i * 2 + j)));

    array<int, 3> c{ { { 0, 1, 2 }, { 3, 4, 5 } }, { { 6, 7, 8 }, { 9, 10, 11 } } };
    TEST_ARRAY_INDEX_LOOP(i, c, 0,
                          TEST_ARRAY_INDEX_LOOP(j, c, 1,
                                                TEST_ARRAY_INDEX_LOOP(k, c, 2,
                                                                      EXPECT_EQ((c[{i, j, k}]), i * 6 + j * 3 + k))));
    TEST_ARRAY_INDEX_LOOP(i, c, 0,
                          TEST_ARRAY_INDEX_LOOP(j, c, 1,
                                                TEST_ARRAY_INDEX_LOOP(k, c, 2,
                                                                      EXPECT_EQ(c[i][j][k], i * 6 + j * 3 + k))));
    TEST_ARRAY_INDEX_LOOP(i, c, 0,
                          TEST_ARRAY_INDEX_LOOP(j, c, 1,
                                                TEST_ARRAY_INDEX_LOOP(k, c, 2,
                                                                      EXPECT_EQ(c.at(i, j, k), i * 6 + j * 3 + k))));
    TEST_ARRAY_INDEX_LOOP(i, c, 0,
                          TEST_ARRAY_INDEX_LOOP(j, c, 1,
                                                TEST_ARRAY_INDEX_LOOP(k, c, 2,
                                                                      EXPECT_EQ(c.at({i, j, k}), i * 6 + j * 3 + k))));

    EXPECT_THROW(a.at(10), std::out_of_range);
    EXPECT_THROW(b.at(10), std::out_of_range);
    EXPECT_THROW(b.at(0, 10), std::out_of_range);
    EXPECT_THROW(b.at(10, 0), std::out_of_range);
    EXPECT_THROW(c.at(0, 0, 4), std::out_of_range);
    EXPECT_THROW(c.at(2, 0, 0), std::out_of_range);
    EXPECT_THROW(c.at(0, 3, 0), std::out_of_range);
    EXPECT_THROW(c.at(10, 10, 10), std::out_of_range);
}

TEST(ArrayTest, ArrayIteration) {
    int i, j;

    array<int, 1> a{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it, a, i, EXPECT_EQ(it, i));

    array<int, 2> b{ { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 } };

    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it1, b, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it2, it1, j, EXPECT_EQ(it2, i * 3 + j)));

    TEST_ARRAY_LOOP_WITH_COUNTER(auto, it1, b, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(auto&, it2, it1, j, it2 *= -1));

    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it1, b, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it2, it1, j, EXPECT_EQ(it2, -i * 3 - j)));

    TEST_ARRAY_REVERSE_LOOP_WITH_COUNTER(it, a, i, EXPECT_EQ(*it, 9 - i));

    TEST_ARRAY_REVERSE_LOOP_WITH_COUNTER(it1, b, i,
                                         TEST_ARRAY_REVERSE_LOOP_WITH_COUNTER(it2, (*it1), j,
                                                                              EXPECT_EQ(*it2, -8 + i * 3 + j)));
}

TEST(ArrayTest, ArrayFlatten) {
    int i;

    array<int, 2> a{ { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 } };

    const auto b = a.flatten();
    EXPECT_EQ(b.size(), 9);
    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it, b, i, EXPECT_EQ(it, i));
}

TEST(ArrayTest, ArrayReshape) {
    int i, j, k;

    array<int, 1> a{ 0, 1, 2, 3, 4, 5, 6, 7, 8 };

    const auto b = a.reshape(3, 3);
    EXPECT_EQ(b.size(), 3);
    EXPECT_EQ(b.size(1), 3);
    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it1, b, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it2, it1, j, EXPECT_EQ(it2, i * 3 + j)));

    const auto c = a.reshape(3, -1);
    EXPECT_EQ(c.size(), 3);
    EXPECT_EQ(c.size(1), 3);
    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it1, c, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it2, it1, j, EXPECT_EQ(it2, i * 3 + j)));

    const auto d = a.reshape(-1, 3);
    EXPECT_EQ(d.size(), 3);
    EXPECT_EQ(d.size(1), 3);
    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it1, d, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it2, it1, j, EXPECT_EQ(it2, i * 3 + j)));

    const auto e = a.reshape(3, -1, 3);
    EXPECT_EQ(e.size(), 3);
    EXPECT_EQ(e.size(1), 1);
    EXPECT_EQ(e.size(2), 3);
    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it1, e, i,
                                 TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it2, it1, j,
                                                              TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it3, it2, k,
                                                                                           EXPECT_EQ(it3, i * 3 + j + k))));

    const auto f = d.reshape(-1);
    EXPECT_EQ(f.size(), 9);
    TEST_ARRAY_LOOP_WITH_COUNTER(const auto&, it, f, i, EXPECT_EQ(it, i));

    EXPECT_THROW(a.reshape(-1, -1, 9), std::logic_error);
}

TEST(ArrayTest, ArrayInitialization) {
    array<int, 1> a(10);
    a.fill(42);

    TEST_ARRAY_LOOP(const auto&, it, a, EXPECT_EQ(it, 42));

    a = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a[i], i));

    std::vector vector{ 15, 6, 100, 42, 75, 23, -10, 50, 91, 87, 17, 50, 42, 64, 89, 0 };

    EXPECT_THROW(a.assign(vector.begin(), vector.end()), std::logic_error);

    a.assign(vector.begin(), vector.begin() + 10);
    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a[i], vector[i]));

    EXPECT_THROW((array<int, 2>{ { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, { 1, 2, 3 } }), std::logic_error);

    array<int, 2> b{ { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 } };
    TEST_ARRAY_INDEX_LOOP(i, b, 0, TEST_ARRAY_INDEX_LOOP(j, b, 1, EXPECT_EQ((b[{i, j}]), i == 0 ? j : 9 - j )));

    a.assign(b[1]);
    TEST_ARRAY_INDEX_LOOP(i, a, 0, EXPECT_EQ(a[i], (b[{1, i}])));

    array<int, 3> c(1, 3, 10);
    c[{0, 0}] = a;
    c[{0, 1}] = b[0];
    c[{0, 2}] = b[1];

    TEST_ARRAY_INDEX_LOOP(i, c, 2, EXPECT_EQ((c[{0, 0, i}]), a[i]));
    TEST_ARRAY_INDEX_LOOP(i, c, 2, EXPECT_EQ((c[{0, 1, i}]), (b[{0, i}])));
    TEST_ARRAY_INDEX_LOOP(i, c, 2, EXPECT_EQ((c[{0, 2, i}]), (b[{1, i}])));
}

TEST(ArrayTest, AsStrided) {
    array<int, 2> a{
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    const auto b = a.as_strided({ 3 }, a.strides());

    EXPECT_EQ(b[0], 1);
    EXPECT_EQ(b[1], 4);
    EXPECT_EQ(b[2], 7);

    const auto c = a.as_strided( { 3 }, a.strides(), 1);

    EXPECT_EQ(c[0], 2);
    EXPECT_EQ(c[1], 5);
    EXPECT_EQ(c[2], 8);

    const auto d = a.as_strided( { 7, 3 }, { 1, 1 });

    TEST_ARRAY_INDEX_LOOP(i, d, 0,
                          TEST_ARRAY_INDEX_LOOP(j, d, 1,
                                                EXPECT_EQ((d[{i, j}]), i + j + 1)));
}
