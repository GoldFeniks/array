#pragma once
#include <cstddef>
#include <memory>
#include <type_traits>
#include "type_traits.hpp"
#include <cassert>
#include <cstring>
#include <tuple>

namespace feniks {

    template<typename T, size_t D, typename Allocator = std::allocator<T>, bool Owner = true>
    class array {

    private:

        template<bool Const>
        class base_iterator;

    public:

        typedef T data_type;
        typedef std::conditional_t<D == 1, data_type, array<data_type, D - 1, Allocator, false>> value_type;
        typedef Allocator allocator_type;
        typedef size_t size_type;
        typedef std::ptrdiff_t difference_type;
        typedef value_type& reference;
        typedef const value_type const_reference;
        typedef data_type& data_reference;
        typedef const data_type& const_data_reference;

        static constexpr auto dimensions = D;

        using iterator = base_iterator<false>;
        using const_iterator = base_iterator<true>;

        array() = delete;

        template<typename... S, typename = std::enable_if_t<is_convertible_all_v<size_type, S...> && (sizeof...(S) == D) && Owner>>
        explicit array(S... sizes) : sizes_(make_sizes(sizes...)), offsets_(make_offsets()) {
            allocate();
        }

        template<bool O, typename C = void, typename = std::enable_if_t<Owner, C>>
        explicit array(const array<T, D, Allocator, O>& other) {
            allocate_sizes();
            *this = other;
        };

        template<bool O, typename C = void, typename = std::enable_if_t<Owner, C>>
        array& operator=(const array<T, D, Allocator, O>& other) {
            copy(other);
            return *this;
        }

        template<typename C = void, typename = std::enable_if_t<Owner, C>>
        explicit array(array<T, D, Allocator, true>&& other) {
            allocate_sizes();
            *this = std::move(other);
        }

        template<typename C = void, typename = std::enable_if_t<Owner, C>>
        array& operator=(array<T, D, Allocator, true>&& other) {
            std::swap(sizes_, other.sizes_);
            std::swap(offsets_, other.offsets_);
            std::swap(data_begin_, other.data_begin_);
            std::swap(data_end_, other.data_end_);
            std::swap(allocator_, other.allocator_);
            return *this;
        }

        template<typename I = size_type, typename = std::enable_if_t<D == 1, I>>
        auto& operator[](const I& index) {
            return data_begin_[index];
        }

        template<typename I = size_type, typename = std::enable_if_t<D != 1, I>>
        auto operator[](const I& index) {
            return array<T, D - 1, Allocator, false>(data_begin_ + index * *offsets_, sizes_ + 1, offsets_ + 1);
        }

        template<typename I = size_type, typename = std::enable_if_t<D == 1, I>>
        const auto& operator[](const I& index) const {
            return data_begin_[index];
        }

        template<typename I = size_type, typename = std::enable_if_t<D != 1, I>>
        const auto operator[](const I& index) const {
            return array<T, D - 1, Allocator, false>(data_begin_ + index * *offsets_, sizes_ + 1, offsets_ + 1);
        }

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) == D)>>
        auto& at(I... index) {
            return *std::get<0>(index_<I...>(index...));
        };

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) < D)>>
        auto at(I... index) {
            auto ind = index_(index...);
            return array<T, D - sizeof...(I), Allocator, false>(std::get<0>(ind), sizes_ + sizeof...(I), std::get<1>(ind));
        };

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) == D)>>
        const auto& at(I... index) const {
            return *std::get<0>(index_<I...>(index...));
        };

        template<typename... I, typename = std::enable_if_t<is_convertible_all_v<size_type, I...> && (sizeof...(I) < D)>>
        const auto at(I... index) const {
            auto ind = index_(index...);
            return array<T, D - sizeof...(I), Allocator, false>(std::get<0>(ind), sizes_ + sizeof...(I), std::get<1>(ind));
        };

        ~array() {
            if (Owner) {
                allocator_->deallocate(data_begin_, full_size());
                delete[] sizes_;
                delete[] offsets_;
            }
        }

        auto size(size_type n = 0) const {
            return sizes_[n];
        }

        auto full_size() const {
            return *sizes_ * *offsets_;
        }

        iterator begin() noexcept {
            return iterator(data_begin_, sizes_, offsets_);
        }

        iterator end() noexcept {
            return iterator(data_end_, sizes_, offsets_);
        }

        const_iterator begin() const noexcept {
            return const_iterator(data_begin_, sizes_, offsets_);
        }

        const_iterator end() const noexcept {
            return const_iterator(data_end_, sizes_, offsets_);
        }

    private:

        template<typename, size_t, typename, bool>
        friend class array;

        template<typename S = size_type>
        array(data_type* data, S* sizes, std::enable_if_t<!Owner, S*> offsets) :
                data_begin_(data), data_end_(data + *sizes * *offsets), sizes_(sizes), offsets_(offsets) {}

        inline void allocate_sizes() {
            sizes_ = new size_type[D];
            offsets_ = new size_type[D];
            std::memset(sizes_, 0, sizeof(size_type) * D);
            std::memset(offsets_, 0, sizeof(size_type) * D);
        }

        inline void allocate() {
            data_begin_ = allocator_->allocate(full_size());
            data_end_ = data_begin_ + full_size();
        }

        template<bool O>
        void copy(const array<T, D, Allocator, O>& other) {
            const auto old_size = full_size();
            const auto new_size = other.full_size();
            if (Owner) {
                allocator_->deallocate(data_begin_, old_size);
                std::copy(other.sizes_, other.sizes_ + D, sizes_);
                std::copy(other.offsets_, other.offsets_ + D, offsets_);
                allocate();
            }
            else
                for (size_t i = 0; i < D; ++i)
                    assert(sizes_[i] == other.sizes_[i]);
            for (size_type i = 0; i < new_size; ++i)
                if (std::is_default_constructible_v<T>)
                    data_begin_[i] = other.data_begin_[i];
                else
                    new (data_begin_ + i) T(other.data_begin_[i]);
        }

        template<typename... I>
        auto index_(I... index) const {
            auto o = offsets_, d = data_begin_;
            for (const auto& it : { size_type(index)... })
                d += *o++ * it;
            return std::tuple(d, o);
        }

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

        size_type *sizes_, *offsets_;
        allocator_type* allocator_ = Owner ? new allocator_type() : nullptr;
        data_type* data_begin_ = nullptr, *data_end_ = nullptr;

        template<bool Const>
        class base_iterator : public std::iterator<std::random_access_iterator_tag, std::conditional_t<Const, const value_type, value_type>> {

        public:

            base_iterator() = default;
            base_iterator(const base_iterator& other) = default;
            base_iterator(base_iterator&& other) noexcept = default;
            ~base_iterator() = default;

            bool operator==(const base_iterator& other) { return data_ == other.data_; }
            bool operator!=(const base_iterator& other) { return !(*this == other); }

            template<typename = std::enable_if_t<1 < D>>
            value_type operator*() { return array<T, D - 1, Allocator, false>(data_, size_ + 1, offset_ + 1); }

            template<typename = std::enable_if_t<1 < D>>
            value_type operator->() { return array<T, D - 1, Allocator, false>(data_, size_ + 1, offset_ + 1); }

            template<typename = std::enable_if_t<1 == D>>
            reference operator*() { return *data_; }

            template<typename = std::enable_if_t<1 == D>>
            reference operator->() { return data_; }

            base_iterator& operator++() {
                data_ += *offset_;
                return *this;
            }

            const base_iterator operator++(int) {
                const auto temp = *this;
                (*this)++;
                return temp;
            }

            base_iterator& operator--() {
                data_ -= *offset_;
                return *this;
            }

            const base_iterator operator--(int) {
                const auto temp = *this;
                (*this)--;
                return temp;
            }

            base_iterator& operator+=(const size_t n) {
                data_ += *offset_ * n;
                return *this;
            }

            base_iterator& operator-=(const size_t n) {
                data_ -= *offset_ * n;
                return *this;
            }

            base_iterator operator+(const size_t n) const {
                auto temp = *this;
                temp += n;
                return temp;
            }

            base_iterator operator-(const size_t n) const {
                auto temp = *this;
                temp -= n;
                return temp;
            }

            size_type operator-(const base_iterator& other) const {
                return (data_ - other.data_) / *offset_;
            }

            bool operator<(const base_iterator& other) const {
                return data_ < other.data_;
            }

            bool operator>(const base_iterator& other) const {
                return data_ > other.data_;
            }

            bool operator<=(const base_iterator& other) const {
                return !(*this > other);
            }

            bool operator>=(const base_iterator& other) const {
                return !(*this < other);
            }

            template<typename = std::enable_if_t<1 < D>>
            value_type operator[](const size_type index) {
                return *(*this + index);
            };

            template<typename = std::enable_if_t<1 == D>>
            reference operator[](const size_type index) {
                return *(*this + index);
            };

        private:

            template<typename, size_t, typename, bool>
            friend class array;

            base_iterator(T* data, size_type* size, size_type* offset) : data_(data), size_(size), offset_(offset) {}

            data_type* data_ = nullptr;
            size_type* size_ = nullptr, *offset_ = nullptr;

        };

    };

}