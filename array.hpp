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

        template<typename... S, typename = std::enable_if_t<is_convertible_all_v<size_type, S...> && (sizeof...(S) == D) && Owner>>
        explicit array(S... sizes) : sizes_(make_sizes(sizes...)), offsets_(make_offsets()) {
            const auto mul = offsets_[0] * sizes_[0];
            data_begin_ = allocator_->allocate(mul);
            data_end_ = data_begin_ + mul;
        }

        template<typename S = size_type>
        array(data_type* data, const S* sizes, std::enable_if_t<!Owner, const S*> offsets) :
                data_begin_(data), data_end_(data + *sizes + *offsets), sizes_(sizes), offsets_(offsets) {}

        template<typename I = size_type>
        auto& operator[](std::enable_if_t<D == 1, const I&> index) {
            return data_begin_[index];
        }

        template<typename I = size_type>
        auto operator[](std::enable_if_t<D != 1, const I&> index) {
            return array<T, D - 1, Allocator, false>(data_begin_ + index * *offsets_, sizes_ + 1, offsets_ + 1);
        }

        template<typename I = size_type>
        const auto& operator[](std::enable_if_t<D == 1, const I&> index) const {
            return data_begin_[index];
        }

        template<typename I = size_type>
        const auto operator[](std::enable_if_t<D != 1, const I&> index) const {
            return array<T, D - 1, Allocator, false>(data_begin_ + index * *offsets_, sizes_ + 1, offsets_ + 1);
        }

        template<typename I = size_t>
        auto& at(std::enable_if_t<D == 1, const I> index) {
            if (index >= size())
                throw std::out_of_range("Array index out of range");
            return data_begin_[index];
        }

        template<typename I = size_t>
        auto at(std::enable_if_t<D != 1, const I> index) {
            if (index >= size())
                throw std::out_of_range("Array index out of range");
            return array<T, D - 1, Allocator, false>(data_begin_ + index * *offsets_, sizes_ + 1, offsets_ + 1);
        }

        template<typename I = size_t>
        const auto& at(std::enable_if_t<D == 1, const I> index) const {
            if (index >= size())
                throw std::out_of_range("Array index out of range");
            return data_begin_[index];
        }

        template<typename I = size_t>
        const auto at(std::enable_if_t<D != 1, const I> index) const {
            if (index >= size())
                throw std::out_of_range("Array index out of range");
            return array<T, D - 1, Allocator, false>(data_begin_ + index * *offsets_, sizes_ + 1, offsets_ + 1);
        }

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) == D)>>
        auto& at(I... index) {
            auto o = offsets_, d = data_begin_;
            for (const auto& it : { size_type(index)... })
                d += *o++ * it;
            return *d;
        };

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) < D)>>
        auto at(I... index) {
            auto o = offsets_, d = data_begin_;
            for (const auto& it : { size_type(index)... })
                d += *o++ * it;
            return array<T, D - sizeof...(I), Allocator, false>(d, sizes_ + sizeof...(I), o);
        };

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) == D)>>
        const auto& at(I... index) const {
            auto o = offsets_, d = data_begin_;
            for (const auto& it : { size_type(index)... })
                d += *o++ * it;
            return *d;
        };

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) < D)>>
        const auto at(I... index) const {
            auto o = offsets_, d = data_begin_;
            for (const auto& it : { size_type(index)... })
                d += *o++ * it;
            return array<T, D - sizeof...(I), Allocator, false>(d, sizes_ + sizeof...(I), o);
        };

        ~array() {
            if (Owner) {
                for (size_t i = 0; i < *sizes_ * *offsets_; ++i)
                    allocator_->destroy(data_begin_ + i);
                allocator_->deallocate(data_begin_, *sizes_ * *offsets_);
                delete[] sizes_;
                delete[] offsets_;
            }
        }

        auto size(size_type n = 0) const {
            return sizes_[n];
        }

    private:

        template<typename... S>
        auto make_sizes(S... sizes) {
            auto result = new size_type[D];
            auto p = result;
            for (const auto& it : { size_type(sizes)... })
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