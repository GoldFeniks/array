#pragma once
#include <cstddef>
#include <memory>
#include <type_traits>
#include "type_traits.hpp"

namespace feniks {

    template<typename T, size_t D, typename Allocator = std::allocator<T>, bool Owner = true>
    class array {

    public:

        typedef T data_type;
        typedef std::conditional_t<D == 1, data_type, array<T, D - 1, Allocator>> value_type;
        typedef Allocator allocator_type;
        typedef size_t size_type;
        typedef std::ptrdiff_t difference_type;
        typedef value_type& reference;
        typedef const value_type const_reference;
        typedef data_type& data_reference;
        typedef const data_type& const_data_reference;

        static constexpr auto dimensions = D;

        template<typename... S, typename = std::enable_if_t<is_convertible_all_v<size_t, S...> && (sizeof...(S) == D) && Owner>>
        explicit array(S... sizes) : sizes_(make_sizes(sizes...)), offsets_(make_offsets()) {
            const auto mul = offsets_[0] * sizes_[0];
            data_begin_ = allocator_->allocate(mul);
            data_end_ = data_begin_ + mul;
        }

        ~array() {
            if (Owner) {
                for (size_t i = 0; i < *sizes_ * *offsets_; ++i)
                    allocator_->destroy(data_begin_ + i);
                allocator_->deallocate(data_begin_, *sizes_ * *offsets_);
                delete[] sizes_;
                delete[] offsets_;
            }
        }

        auto size() {
            return *sizes_;
        }

    private:

        template<typename... S>
        auto make_sizes(S... sizes) {
            auto result = new size_type[D];
            auto p = result;
            for (const auto it : { size_type(sizes)... })
                *p++ = it;
            return result;
        }

        auto make_offsets() {
            auto result = new size_type[D];
            auto mul = sizes_[D - 1];
            result[D - 1] = 1;
            for (size_type i = 2; i <= D; ++i) {
                result[D - i] = mul;
                mul *= sizes_[D - i];
            }
            return result;
        }

        const size_type *sizes_, *offsets_;
        allocator_type* allocator_ = Owner ? new allocator_type() : nullptr;
        T* data_begin_ = nullptr, *data_end_ = nullptr;

    };

}