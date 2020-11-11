#include <type_traits>

#include "gtest/gtest.h"
#include "feniks/array.hpp"
#include "feniks/iterator.hpp"

using namespace feniks;

template<typename, typename>
struct iterator_concept_test {

    static constexpr bool value = false;

};

template<typename I> requires std::random_access_iterator<I>
struct iterator_concept_test<I, std::random_access_iterator_tag> {

    static constexpr bool value = true;

};

template<typename I> requires std::contiguous_iterator<I>
struct iterator_concept_test<I, std::contiguous_iterator_tag> {

    static constexpr bool value = true;

};

template<typename, bool C, typename>
struct iterator_test;

template<typename A, typename T>
struct iterator_test<A, true, T> {

    using iterator = const_iterator<A>;

    static constexpr bool value =
        iterator_concept_test<iterator, T>::value &&
        std::is_same_v<typename iterator::iterator_category, T> &&
        std::is_same_v<typename iterator::pointer,         typename A::const_pointer> &&
        std::is_same_v<typename iterator::reference,       typename A::const_reference> &&
        std::is_same_v<typename iterator::data_type, const typename A::data_type> &&
        std::is_same_v<typename iterator::size_type,       typename A::size_type> &&
        std::is_same_v<typename iterator::value_type,const typename A::value_type> &&
        std::is_same_v<typename iterator::difference_type, typename A::difference_type>;

};

template<typename A, typename T>
struct iterator_test<A, false, T> {

    using iterator = ::iterator<A>;

    static constexpr bool value =
            iterator_concept_test<iterator, T>::value &&
            std::is_same_v<typename iterator::iterator_category, T> &&
            std::is_same_v<typename iterator::pointer,         typename A::pointer> &&
            std::is_same_v<typename iterator::reference,       typename A::reference> &&
            std::is_same_v<typename iterator::data_type,       typename A::data_type> &&
            std::is_same_v<typename iterator::size_type,       typename A::size_type> &&
            std::is_same_v<typename iterator::value_type,      typename A::value_type> &&
            std::is_same_v<typename iterator::difference_type, typename A::difference_type>;

};

template<typename A, bool C, typename T>
static constexpr bool iterator_test_v = iterator_test<A, C, T>::value;

TEST(ArrayTest, IteratorCategoryAndMemberTypes) {
// TODO fix iterator tag compliance

//    EXPECT_TRUE((iterator_test_v<array<int, 1>, false, std::contiguous_iterator_tag>));
//    EXPECT_TRUE((iterator_test_v<array<int, 1>, true, std::contiguous_iterator_tag>));

//    EXPECT_TRUE((iterator_test_v<array<int, 2>, false, std::random_access_iterator_tag>));
//    EXPECT_TRUE((iterator_test_v<array<int, 2>, true, std::random_access_iterator_tag>));
}
