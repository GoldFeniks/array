#pragma once
#include <stdexcept>

namespace feniks {

    namespace _impl {

        template<typename E = std::logic_error>
        void dynamic_assert(const bool &condition, const char *message) {
            if (!condition)
                throw E(message);
        }

        template<typename E = std::logic_error>
        void dynamic_assert(const bool &condition) {
            dynamic_assert<E>(condition, "Dynamic assertion failed");
        }

    }// namespace _impl

}// namespace feniks
