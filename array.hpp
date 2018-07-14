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

        using data_type = T;
        using value_type = std::conditional_t<D == 1, data_type, array<data_type, D - 1, Allocator, false>>;
        using allocator_type = Allocator;
        using size_type = size_t;
        using difference_type = std::ptrdiff_t;
        using reference = value_type&;
        using const_reference = const value_type&;
        using data_reference = data_type&;
        using const_data_reference = const data_type&;

        static constexpr auto dimensions = D;

        using iterator = base_iterator<false>;
        using const_iterator = base_iterator<true>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

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
        auto& operator=(const array<T, D, Allocator, O>& other) {
            copy(other);
            return *this;
        }

        template<typename C = void, typename = std::enable_if_t<Owner, C>>
        explicit array(array<T, D, Allocator, true>&& other) {
            allocate_sizes();
            *this = std::move(other);
        }

        template<typename C = void, typename = std::enable_if_t<Owner, C>>
        auto& operator=(array<T, D, Allocator, true>&& other) {
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

        auto begin() noexcept {
            return iterator(data_begin_, sizes_, offsets_);
        }

        auto end() noexcept {
            return iterator(data_end_, sizes_, offsets_);
        }

        auto begin() const noexcept {
            return const_iterator(data_begin_, sizes_, offsets_);
        }

        auto end() const noexcept {
            return const_iterator(data_end_, sizes_, offsets_);
        }

        auto rbegin() noexcept {
            return std::make_reverse_iterator(iterator(data_end_, sizes_, offsets_));
        }

        auto rend() noexcept {
            return std::make_reverse_iterator(iterator(data_begin_, sizes_, offsets_));
        }

        auto rbegin() const noexcept {
            return std::make_reverse_iterator(const_iterator(data_end_, sizes_, offsets_));
        }

        auto rend() const noexcept {
            return std::make_reverse_iterator(const_iterator(data_begin_, sizes_, offsets_));
        }

        auto fbegin() noexcept {
            return typename array<T, 1, Allocator, Owner>::iterator(data_begin_, sizes_ + D - 1, offsets_ + D - 1);
        }

        auto fend() noexcept {
            return typename array<T, 1, Allocator, Owner>::iterator(data_end_, sizes_ + D - 1, offsets_ + D - 1);
        }

        auto fbegin() const noexcept {
            return typename array<T, 1, Allocator, Owner>::const_iterator(data_begin_, sizes_ + D - 1, offsets_ + D - 1);
        }

        auto fend() const noexcept {
            return typename array<T, 1, Allocator, Owner>::const_iterator(data_end_, sizes_ + D - 1, offsets_ + D - 1);
        }

        auto rfbegin() noexcept {
            return std::make_reverse_iterator(typename array<T, 1, Allocator, Owner>::iterator(data_end_, sizes_ + D - 1, offsets_ + D - 1));
        }

        auto rfend() noexcept {
            return std::make_reverse_iterator(typename array<T, 1, Allocator, Owner>::iterator(data_begin_, sizes_ + D - 1, offsets_ + D - 1));
        }

        auto rfbegin() const noexcept {
            return std::make_reverse_iterator(typename array<T, 1, Allocator, Owner>::const_iterator(data_end_, sizes_ + D - 1, offsets_ + D - 1));
        }

        auto rfend() const noexcept {
            return std::make_reverse_iterator(typename array<T, 1, Allocator, Owner>::const_iterator(data_begin_, sizes_ + D - 1, offsets_ + D - 1));
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

            using base = std::iterator<std::random_access_iterator_tag, std::conditional_t<Const, const value_type, value_type>>;

            base_iterator() = default;
            base_iterator(const base_iterator& other) = default;
            base_iterator(base_iterator&& other) noexcept = default;
            ~base_iterator() = default;

            bool operator==(const base_iterator& other) { return data_ == other.data_; }
            bool operator!=(const base_iterator& other) { return !(*this == other); }

            template<typename = std::enable_if_t<1 < D>>
            typename base::value_type operator*() { return array<T, D - 1, Allocator, false>(data_, size_ + 1, offset_ + 1); }

            template<typename = std::enable_if_t<1 == D>>
            typename base::reference operator*() { return *data_; }

            template<typename = std::enable_if_t<1 == D>>
            typename base::pointer operator->() { return data_; }

            template<typename = std::enable_if_t<1 < D>>
            const typename base::value_type operator*() const { return array<T, D - 1, Allocator, false>(data_, size_ + 1, offset_ + 1); }

            template<typename = std::enable_if_t<1 < D>>
            const typename base::value_type operator->() const { return array<T, D - 1, Allocator, false>(data_, size_ + 1, offset_ + 1); }

            template<typename = std::enable_if_t<1 == D>>
            const typename base::reference operator*() const { return *data_; }

            template<typename = std::enable_if_t<1 == D>>
            const typename base::pointer operator->() const { return data_; }

            auto& operator++() {
                data_ += *offset_;
                return *this;
            }

            const auto operator++(int) {
                const auto temp = *this;
                (*this)++;
                return temp;
            }

            auto& operator--() {
                data_ -= *offset_;
                return *this;
            }

            const auto operator--(int) {
                const auto temp = *this;
                (*this)--;
                return temp;
            }

            auto& operator+=(const size_t n) {
                data_ += *offset_ * n;
                return *this;
            }

            auto& operator-=(const size_t n) {
                data_ -= *offset_ * n;
                return *this;
            }

            auto operator+(const size_t n) const {
                auto temp = *this;
                temp += n;
                return temp;
            }

            auto operator-(const size_t n) const {
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
            typename base::value_type operator[](const size_type index) {
                return *(*this + index);
            };

            template<typename = std::enable_if_t<1 == D>>
            typename base::reference operator[](const size_type index) {
                return *(*this + index);
            };

            template<typename = std::enable_if_t<1 < D>>
            const typename base::value_type operator[](const size_type index) const {
                return *(*this + index);
            };

            template<typename = std::enable_if_t<1 == D>>
            const typename base::reference operator[](const size_type index) const {
                return *(*this + index);
            };

        private:

            template<typename, size_t, typename, bool>
            friend class array;

            base_iterator(data_type* data, size_type* size, size_type* offset) : data_(data), size_(size), offset_(offset) {}

            data_type* data_ = nullptr;
            size_type* size_ = nullptr, *offset_ = nullptr;

        };

    };

}