cmake_minimum_required(VERSION 3.18)
project(array_tests)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

set(PROJECT_DIR ${PROJECT_SOURCE_DIR}/../)

find_package(GTest REQUIRED)
include_directories(${PROJECT_DIR}/include)

add_executable(array_tests
        array_tests.cpp
        test_array.cpp
        test_iterator.cpp
        test_member_types.cpp
        ${PROJECT_DIR}/include/feniks/array.hpp
        ${PROJECT_DIR}/include/feniks/iterator.hpp
        ${PROJECT_DIR}/include/feniks/type_traits.hpp
        ${PROJECT_DIR}/include/feniks/shared_data.hpp)

target_link_libraries(array_tests GTest::GTest GTest::Main)

enable_testing()
add_test(array_tests "./array_tests")
