#pragma once
#include <span>
#include <tuple>
#include <memory>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <type_traits>
#include "iterator.hpp"
#include "type_traits.hpp"

namespace feniks {

    namespace _impl {

        template<typename, size_t, typename Allocator, bool>
        class array;

        template<typename T, size_t N, typename Allocator, bool Owner>
        struct array_type {

            using type = array<std::remove_reference_t<T>, N, Allocator, Owner>;

        };

        template<typename T, typename Allocator, bool Owner>
        struct array_type<T, 0, Allocator, Owner> {

            using type = T;

        };

        template<typename T, size_t N, typename Allocator, bool Owner>
        using array_type_t = typename array_type<T, N, Allocator, Owner>::type;

        template<typename T, size_t D = 1, typename Allocator = std::allocator<T>, bool Owner = true>
        class array {

        public:

            static constexpr auto dimensions = D;

            using data_type              = T;
            using size_type              = std::conditional_t<Owner, size_t, const size_t>;
            using value_type             = array_type_t<T, D - 1, Allocator, false>;
            using allocator_type         = Allocator;
            using difference_type        = std::ptrdiff_t;

            using pointer                = typename std::allocator_traits<Allocator>::pointer;
            using const_pointer          = typename std::allocator_traits<Allocator>::const_pointer;

            using reference              = value_type&;
            using const_reference        = const value_type&;

            using data_reference         = data_type&;
            using const_data_reference   = const data_type&;

            using iterator               = feniks::iterator<array>;
            using const_iterator         = feniks::const_iterator<array<const T, D, Allocator, Owner>>;

            using reverse_iterator       = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;

            array() = delete;

            ~array() {
                if constexpr (Owner) {
                    _allocator->deallocate(_data_begin, full_size());
                    delete _allocator;
                    delete[] _sizes;
                    delete[] _strides;
                }
            }

            template<typename... S, typename = std::enable_if_t<(std::is_convertible_v<S, size_type> && ...) && (sizeof...(S) == D) && Owner>>
            explicit array(const S&... sizes)
                    : _sizes(array::sizes<S...>::make(sizes...)), _strides(strides<S...>::make(sizes...)) {
                _allocate();
            }

            template<typename V, bool O, typename = std::enable_if_t<Owner && std::is_same_v<std::remove_const_t<V>, data_type>>>
            explicit array(const array<V, D, Allocator, O>& other) {
                _allocate_sizes();
                *this = other;
            }

            template<typename V, bool O, typename = std::enable_if_t<Owner && std::is_same_v<std::remove_const_t<V>, data_type>>>
            explicit array(array<V, D, Allocator, O>&& other) {
                _allocate_sizes();
                *this = std::move(other);
            }

            array(const std::initializer_list<array_type_t<T, D - 1, Allocator, true>>& data) {
                static_assert(Owner, "Cannot create view this way");

                if constexpr (D == 1) {
                    _sizes = sizes<decltype(data.size())>::make(data.size());
                    _strides = strides<decltype(data.size())>::make(data.size());
                    _allocate();
                    std::copy(data.begin(), data.end(), _data_begin);
                } else {
                    _allocate_sizes();
                    auto it = data.begin();
                    std::copy(it->_sizes, it->_sizes + D - 1, _sizes + 1);
                    std::copy(it->_strides, it->_strides + D - 1, _strides + 1);
                    _sizes[0] = data.size();
                    _sizes[D] = full_size();
                    _strides[0] = _sizes[1] * _strides[1];

                    _allocate();
                    auto d = _data_begin;

                    for (; it != data.end(); ++it, d += _strides[0]) {
                        _all_equal(_sizes + 1, it->_sizes, std::make_index_sequence<D - 1>{});
                        std::copy(it->_data_begin, it->_data_end, d);
                    }
                }
            }

            template<typename V, typename = std::enable_if_t<std::is_same_v<std::remove_const_t<V>, data_type>>>
            array& operator=(const array<V, D, Allocator, Owner>& other) {
                _copy(other);
                return *this;
            }

            array& operator=(array&& other) {
                _move(std::move(other));
                return *this;
            }

            array* operator->() {
                return this;
            }

            const array* operator->() const {
                return *this;
            }

            size_type full_size() const {
                return _full_size(std::make_index_sequence<D>{});
            }

            size_type size() const {
                return *_sizes;
            }

            size_type size(const size_type& n) const {
                return _sizes[n];
            }

            template<bool O, typename V, typename = std::enable_if_t<std::is_same_v<std::remove_const_t<V>, data_type>>>
            void assign(const array<V, D, Allocator, O>& other) {
                assert(_all_equal(_sizes, other._sizes));
                std::copy(other._data_begin, other._data_end, _data_begin);
            }

            array_type_t<T&, D - 1, Allocator, false> operator[](const size_type& index) {
                if constexpr (D == 1)
                    return _data_begin[index];
                else
                    return value_type(_data_begin + index * *_strides, _sizes + 1, _strides + 1);
            }

            array_type_t<const T&, D - 1, Allocator, false> operator[](const size_type& index) const {
                if constexpr (D == 1)
                    return _data_begin[index];
                else
                    return array<const data_type, D - 1, Allocator, false>(_data_begin + index * *_strides, _sizes + 1, _strides + 1);
            }

            array_type_t<T&, D - 1, Allocator, false> at(const size_type& index) {
                assert(index >= 0 && index < size());
                return (*this)[index];
            }

            array_type_t<const T&, D - 1, Allocator, false> at(const size_type& index) const {
                assert(index >= 0 && index < size());
                return (*this)[index];
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<T&, D - N, Allocator, false> operator[](const size_type(&index)[N]) {
                auto d = _index(index);
                if constexpr (D == N)
                    return *d;


                else
                    return array_type_t<T&, D - N, Allocator, false>(d, _sizes + N, _strides + N);
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<const T&, D - N, Allocator, false> operator[](const size_type(&index)[N]) const {
                auto d = _index(index);
                if constexpr (D == N)
                    return *d;
                else
                    return array<const data_type, D - N, Allocator, false>(d, _sizes + N, _strides + N);
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<T&, D - N, Allocator, false> at(const size_type(&index)[N]) {
                assert(_check_index(index));
                return (*this)[index];
            }

            template<typename... S, typename = std::enable_if_t<(sizeof...(S) <= D) && (std::is_convertible_v<S, size_type> && ...)>>
            array_type_t<T&, D - sizeof...(S), Allocator, false> at(const S&... index) {
                return this->at({ size_type(index) ... });
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            array_type_t<const T&, D - N, Allocator, false> at(const size_type(&index)[N]) const {
                assert(_check_index(index));
                return (*this)[index];
            }

            template<typename... S, typename = std::enable_if_t<(sizeof...(S) <= D) && (std::is_convertible_v<S, size_type> && ...)>>
            array_type_t<T&, D - sizeof...(S), Allocator, false> at(const S&... index) const {
                return this->at({ size_type(index) ... });
            }

            array<T, 1, Allocator, false> flatten() {
                return array<T, 1, Allocator, false>(_data_begin, _sizes + D, _strides + D - 1);
            }

            array<const T, 1, Allocator, false> flatten() const {
                return array<const T, 1, Allocator, false>(_data_begin, _sizes + D, _strides + D - 1);
            }

            iterator begin() noexcept {
                return iterator(_data_begin, _sizes, _strides);
            }

            iterator end() noexcept {
                return iterator(_data_end, _sizes, _strides);
            }

            const_iterator begin() const noexcept {
                return const_iterator(_data_begin, _sizes, _strides);
            }

            const_iterator end() const noexcept {
                return const_iterator(_data_end, _sizes, _strides);
            }

        private:

            friend class _impl::iterator_helper<D>;

            template<typename, size_t, typename, bool>
            friend class array;

            template<size_t, typename, typename, typename, typename>
            friend class _impl::data_wrapper;

            array(data_type* data, size_type* sizes, size_type* offsets) :
                    _data_begin(data), _data_end(data + *sizes * *offsets), _sizes(sizes), _strides(offsets) {}

            inline void _allocate_sizes() {
                _sizes = new size_type[D + 1];
                _strides = new size_type[D];
                std::memset(_sizes, 0, sizeof(size_type) * (D + 1));
                std::memset(_strides, 0, sizeof(size_type) * D);
            }

            inline void _allocate() {
                _data_begin = _allocator->allocate(full_size());
                _data_end = _data_begin + full_size();
            }

            template<bool O, typename V, typename = std::enable_if_t<std::is_same_v<std::remove_const_t<V>, data_type>>>
            void _copy(const array<V, D, Allocator, O>& other) {
                if constexpr (Owner) {
                    const auto old_size = full_size();
                    const auto new_size = other.full_size();

                    _allocator->deallocate(_data_begin, old_size);
                    std::memcpy(_sizes, other._sizes, sizeof(size_type) * D);
                    std::memcpy(_strides, other._strides, sizeof(size_type) * D);
                    _sizes[D] = full_size();
                    _allocate();

                    for (size_type i = 0; i < new_size; ++i)
                        if (std::is_default_constructible_v<data_type>)
                            _data_begin[i] = other._data_begin[i];
                        else
                            new(_data_begin + i) data_type(other._data_begin[i]);
                } else {
                    static_assert(!O, "Cannot assign data owner to data view. Use view.assign to copy data");

                    _sizes = other._sizes;
                    _strides = other._strides;
                    _data_begin = other._data_begin;
                    _data_end = other._data_end;
                }

            }

            template<bool O>
            void _move(array<data_type, D, Allocator, O>&& other) {
                if constexpr (Owner && !O)
                    _copy(other);
                else {
                    std::swap(_sizes, other._sizes);
                    std::swap(_strides, other._strides);
                    std::swap(_data_begin, other._data_begin);
                    std::swap(_data_end, other._data_end);
                    std::swap(_allocator, other._allocator);
                }
            }

            template<size_t N, typename = std::enable_if_t<N <= D>>
            pointer _index(const size_type(&index)[N]) const {
                auto o = _strides, d = _data_begin;
                for (const auto& it : std::span(index))
                    d += *o++ * it;
                return d;
            }

            template<size_t N>
            bool _check_index(const size_type(&index)[N]) {
                return _check_index(index, std::make_index_sequence<N>{});
            }

            template<size_t N, size_t... I>
            bool _check_index(const size_type(&index)[N], std::index_sequence<I...>) {
                return ((index[I] >= 0 && index[I] < _sizes[I]) &&  ...);
            }


            static bool _all_equal(const size_type* a, const size_type* b) {
                return _all_equal(a, b, std::make_index_sequence<D>{});
            }

            template<size_t... I>
            static bool _all_equal(const size_type* a, const size_type* b, std::index_sequence<I...>) {
                return ((a[I] == b[I]) && ...);
            }

            template<size_t... I>
            size_type _full_size(std::index_sequence<I...>) const {
                return (_sizes[I] * ...);
            }

            template<typename... S>
            struct sizes {

                static size_type* make(const S&... sizes) {
                    return make(sizes..., std::make_index_sequence<sizeof...(S)>{});
                }

                template<size_t... I>
                static size_type* make(const S&... sizes, std::index_sequence<I...>) {
                    auto result = new size_type[sizeof...(S) + 1];
                    ((result[I] = sizes), ...);
                    result[sizeof...(S)] = (result[I] * ...);
                    return result;
                }

            };

            template<typename F, typename... S>
            struct strides {

                static size_type* make(const F&, const S&... sizes) {
                    return make(sizes..., std::make_index_sequence<sizeof...(S)>{});
                }

                template<size_t... I>
                static size_type* make(const S&... sizes, std::index_sequence<I...>) {
                    auto result = new size_type[D];
                    result[D - 1] = 1;

                    auto values = std::make_tuple(sizes..., size_type(1));
                    ((result[D - I - 2] = result[D - I - 1] * std::get<D - I - 2>(values)), ...);
                    return result;
                }

            };

            size_type *_sizes{}, *_strides{};
            data_type* _data_begin = nullptr, *_data_end = nullptr;
            allocator_type* _allocator = Owner ? new allocator_type() : nullptr;

        };

    }

    template<typename T, size_t N = 1, typename Allocator = std::allocator<T>, bool Owner = true>
    using array = typename _impl::array_type_t<T, N, Allocator, Owner>;

}