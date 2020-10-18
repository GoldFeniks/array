cmake_minimum_required(VERSION 3.18)
project(array_test)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)

set(PROJECT_DIR ${PROJECT_SOURCE_DIR}/../)


find_package(GTest REQUIRED)
include_directories(${PROJECT_SOURCE_DIR}/include)

add_executable(array_test
    test.cpp
    ${PROJECT_DIR}/include/feniks/array.hpp
    ${PROJECT_DIR}/include/feniks/type_traits.hpp
)

target_link_libraries(array_test GTest::GTest GTest::Main)
