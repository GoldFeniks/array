#include <type_traits>

#include "gtest/gtest.h"
#include "feniks/array.hpp"

using namespace feniks;

TEST(ArrayTest, MemberTypes) {
    EXPECT_TRUE((std::is_same_v<array<int, 1>::data_type, int>));
    EXPECT_TRUE((std::is_same_v<array<int, 2>::data_type, int>));
    EXPECT_TRUE((std::is_same_v<array<double, 1>::data_type, double>));

    EXPECT_TRUE((std::is_same_v<array<int, 1>::value_type, int>));
    EXPECT_TRUE((std::is_same_v<array<int, 2>::value_type, array<int, 1>>));
    EXPECT_TRUE((std::is_same_v<array<int, 3>::value_type, array<int, 2>>));

    EXPECT_TRUE((std::is_same_v<array<int, 1>::size_type, size_t>));
    EXPECT_TRUE((std::is_same_v<array<int, 1>::difference_type, std::ptrdiff_t>));

    EXPECT_TRUE((std::is_same_v<array<int, 1>::reference, int&>));
    EXPECT_TRUE((std::is_same_v<array<int, 1>::const_reference, const int&>));
    EXPECT_TRUE((std::is_same_v<array<int, 2>::reference, array<int, 1>>));
    EXPECT_TRUE((std::is_same_v<array<int, 3>::reference, array<int, 2>>));
    EXPECT_TRUE((std::is_same_v<array<int, 2>::const_reference, array<const int, 1>>));
    EXPECT_TRUE((std::is_same_v<array<int, 3>::const_reference, array<const int, 2>>));

    EXPECT_TRUE((std::is_same_v<array<int, 1>::data_reference, int&>));
    EXPECT_TRUE((std::is_same_v<array<double, 1>::data_reference, double&>));
    EXPECT_TRUE((std::is_same_v<array<int, 1>::const_data_reference, const int&>));
    EXPECT_TRUE((std::is_same_v<array<double, 1>::const_data_reference, const double&>));

    EXPECT_TRUE((std::is_same_v<array<int, 1>::pointer, int*>));
    EXPECT_TRUE((std::is_same_v<array<double, 1>::pointer, double*>));

    EXPECT_TRUE((std::is_same_v<array<int, 1>::const_pointer, const int*>));
    EXPECT_TRUE((std::is_same_v<array<double, 1>::const_pointer, const double*>));

    EXPECT_TRUE((array<int, 1>::dimensions == 1));
    EXPECT_TRUE((array<int, 2>::dimensions == 2));
}
