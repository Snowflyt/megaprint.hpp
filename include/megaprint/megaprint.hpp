/*
╔═════════════════════════════════════════════════════════════════════════════╗
║                                                                             ║
║                                          _       _                          ║
║                                         (_)     | |      _____  __    __    ║
║    _ __ ___   ___  __ _  __ _ _ __  _ __ _ _ __ | |_    / ___/_/ /___/ /_   ║
║   | '_ ` _ \ / _ \/ _` |/ _` | '_ \| '__| | '_ \| __|  / /__/_  __/_  __/   ║
║   | | | | | |  __/ (_| | (_| | |_) | |  | | | | | |_   \___/ /_/   /_/      ║
║   |_| |_| |_|\___|\__, |\__,_| .__/|_|  |_|_| |_|\__|                       ║
║                    __/ |     | |                                            ║
║                   |___/      |_|                                            ║
║                                                                             ║
║                                                                             ║
║    A single-header, human-friendly pretty printer for C++ 20,               ║
║    offering color output, auto indentation, aggregate struct inspection     ║
║    with field names, enum support, and a wide range of supported types.     ║
║                                                                             ║
║    Start by using `mp::println(...)` to print everything,                   ║
║    just like `print` in Python or `console.log` in JavaScript.              ║
║                                                                             ║
║    Licensed under MPL-2.0                                                   ║
║                                                                             ║
║    Ge Gao (Snowflyt) <gaoge011022@gmail.com>                                ║
║                                                                             ║
║    https://github.com/Snowflyt/megaprint.hpp                                ║
║                                                                             ║
╚═════════════════════════════════════════════════════════════════════════════╝
*/
#ifndef MEGAPRINT_HPP
#define MEGAPRINT_HPP
#pragma once

#include <algorithm>
#include <any>
#include <array>
#include <cctype>
#include <charconv>
#include <complex>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <deque>
#include <filesystem>
#include <forward_list>
#include <functional>
#include <iomanip>
#include <ios>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <queue>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <version>

#ifdef __cpp_lib_expected
#include <expected>
#endif

#ifdef _WIN32
#define NOMINMAX
#include <windows.h> // For supports_color
#else
#include <unistd.h> // For supports_color
#endif

namespace mp {

namespace detail {

[[nodiscard]] inline auto supports_ansi() -> bool {
#ifdef _WIN32
  // NOLINTNEXTLINE(readability-identifier-naming)
  HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hOut == INVALID_HANDLE_VALUE)
    return false;
  // NOLINTNEXTLINE(readability-identifier-naming)
  DWORD dwMode = 0;
  if (!GetConsoleMode(hOut, &dwMode))
    return false;
  dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  return SetConsoleMode(hOut, dwMode);
#else
  if (!isatty(STDOUT_FILENO))
    return false;
  const char *term = std::getenv("TERM");
  if (!term)
    return false;
  std::string t(term);
  return t != "dumb";
#endif
}

[[nodiscard]] inline auto clean_ansi(const std::string &s) -> std::string {
  std::string res;
  res.reserve(s.size());
  for (std::size_t i = 0; i < s.size(); ++i) {
    if (s[i] == '\x1B') { // ESC
      if (i + 1 < s.size()) {
        if (s[i + 1] == '[') { // CSI sequence
          i += 2;
          // Skip until final byte in '@'–'~'
          while (i < s.size() && (s[i] < '@' || s[i] > '~'))
            ++i;
        } else if (s[i + 1] == ']') { // OSC sequence
          i += 2;
          // Skip until BEL or ESC '\'
          while (i < s.size()) {
            if (s[i] == '\a')
              break;
            if (s[i] == '\x1B' && i + 1 < s.size() && s[i + 1] == '\\') {
              ++i;
              break;
            }
            ++i;
          }
        }
      }
    } else {
      res += s[i];
    }
  }
  return res;
}

enum class function_kind : std::uint8_t { regular_function, member_function, functor };

template <typename T> struct function_info;

// Regular function
template <typename R, typename... Args> struct function_info<R(Args...)> {
  using return_type = R;
  using parameters_type = std::tuple<Args...>;
  static constexpr std::size_t arity = sizeof...(Args);
  static constexpr function_kind kind = function_kind::regular_function;
};

// Member function
template <typename C, typename R, typename... Args>
struct function_info<R (C::*)(Args...)> : function_info<R(Args...)> {
  static constexpr function_kind kind = function_kind::member_function;
};
template <typename C, typename R, typename... Args>
struct function_info<R (C::*)(Args...) const> : function_info<R(Args...)> {
  static constexpr function_kind kind = function_kind::member_function;
};

// Lambda / Functor
template <typename F>
  requires requires { &F::operator(); }
struct function_info<F> : function_info<decltype(&F::operator())> {
  static constexpr function_kind kind = function_kind::functor;
};

// https://stackoverflow.com/a/28796458/21418758
template <typename T, template <typename...> class Ref>
struct is_specialization : std::false_type {};
template <template <typename...> class Ref, typename... Args>
struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
template <typename T, template <typename...> class Ref>
inline constexpr bool is_specialization_v = is_specialization<T, Ref>::value;

template <typename T> struct is_std_array : std::false_type {};
template <typename T, std::size_t N> struct is_std_array<std::array<T, N>> : std::true_type {};
template <typename T> inline constexpr bool is_std_array_v = is_std_array<T>::value;

template <typename T> struct is_pair : std::false_type {};
template <typename T1, typename T2> struct is_pair<std::pair<T1, T2>> : std::true_type {};
template <typename T> inline constexpr bool is_pair_v = is_pair<std::remove_cvref_t<T>>::value;

template <typename T> struct is_tuple : std::false_type {};
template <typename... Ts> struct is_tuple<std::tuple<Ts...>> : std::true_type {};
template <typename T> inline constexpr bool is_tuple_v = is_tuple<std::remove_cvref_t<T>>::value;

// https://rodusek.com/posts/2021/03/09/getting-an-unmangled-type-name-at-compile-time/
template <typename T> consteval auto type_name_array() {
#if defined(__GNUC__) || defined(__clang__)
  constexpr std::string_view s = __PRETTY_FUNCTION__;
  constexpr std::size_t start = s.find_last_of('=') + 2;
  constexpr std::size_t end = s.find_last_of(']');
#elif defined(_MSC_VER)
  constexpr std::string_view s = __FUNCSIG__;
  constexpr std::size_t base_start = s.find("type_name_array<") + 16;
  constexpr std::size_t end = s.rfind(">(void)");
  constexpr std::size_t start = [base_start]() {
    if constexpr (s.substr(base_start).starts_with("struct "))
      return base_start + 7; // Skip "struct "
    else if constexpr (s.substr(base_start).starts_with("class "))
      return base_start + 6; // Skip "class "
    return base_start;
  }();
#else
#error Unsupported compiler
#endif
  constexpr std::string_view name = s.substr(start, end - start);
  return [&name]<std::size_t... I>(std::index_sequence<I...> /*indices*/) {
    return std::array{name[I]...};
  }(std::make_index_sequence<name.size()>{});
}

template <typename T> constexpr auto stored_type_name_array = type_name_array<T>();

template <typename T> consteval auto type_name() -> std::string_view {
  constexpr auto &name_array = stored_type_name_array<T>;
  return std::string_view{name_array.data(), name_array.size()};
}

// A minimal implementation of magic_enum's enum_name: https://github.com/Neargye/magic_enum
static constexpr int ENUM_RANGE_MIN = -128;
static constexpr int ENUM_RANGE_MAX = 128;

template <typename E>
[[nodiscard]] constexpr auto enum_index(E value) -> std::optional<std::size_t> {
  using U = std::underlying_type_t<E>;
  U v = static_cast<U>(value);
  if (v < ENUM_RANGE_MIN || v > ENUM_RANGE_MAX)
    return std::nullopt;
  return static_cast<std::size_t>(v - ENUM_RANGE_MIN);
}

template <typename E, E V> consteval auto enum_type_name_with_name_array() {
#if defined(__GNUC__) || defined(__clang__)
  constexpr std::string_view s = __PRETTY_FUNCTION__;
  constexpr std::size_t start = s.find_last_of('=') + 2;
  constexpr std::size_t end = s.find_last_of(']');
#elif defined(_MSC_VER)
  constexpr std::string_view s = __FUNCSIG__;
  constexpr std::size_t start = s.find_last_of(',') + 1;
  constexpr std::size_t end = s.find_last_of('>');
#else
#error Unsupported compiler
#endif
  constexpr std::string_view name = s.substr(start, end - start);
  return [&name]<std::size_t... I>(std::index_sequence<I...> /*indices*/) {
    return std::array{name[I]...};
  }(std::make_index_sequence<name.size()>{});
}

template <typename E, E V>
inline constexpr auto stored_enum_type_name_with_name_array =
    enum_type_name_with_name_array<E, V>();

template <typename E, E V> consteval auto enum_type_name_with_name() -> std::string_view {
  constexpr auto &name_array = stored_enum_type_name_with_name_array<E, V>;
  return std::string_view{name_array.data(), name_array.size()};
}

template <typename E> constexpr auto enum_type_name_with_name(E value) -> std::string_view {
  if (auto idx = enum_index(value)) {
    constexpr std::size_t N = ENUM_RANGE_MAX - ENUM_RANGE_MIN + 1;
    constexpr auto names = []<std::size_t... I>(std::index_sequence<I...> /*indices*/) {
      return std::array<std::string_view, sizeof...(I)>{
          (enum_type_name_with_name<E, static_cast<E>(ENUM_RANGE_MIN + static_cast<int>(I))>())...};
    }(std::make_index_sequence<N>{});
    return names[*idx];
  }
  return {};
}

template <typename E, E V> consteval auto enum_name_array() {
#if defined(__GNUC__) || defined(__clang__)
  constexpr std::string_view s = __PRETTY_FUNCTION__;
  constexpr std::size_t start = s.rfind("::") + 2;
  constexpr std::size_t end = s.find_last_of(']');
#elif defined(_MSC_VER)
  constexpr std::string_view s = __FUNCSIG__;
  constexpr std::size_t start = s.rfind("::") + 2;
  constexpr std::size_t end = s.find_last_of('>');
#else
#error Unsupported compiler
#endif
  constexpr std::string_view name = s.substr(start, end - start);
  return [&name]<std::size_t... I>(std::index_sequence<I...> /*indices*/) {
    return std::array{name[I]...};
  }(std::make_index_sequence<name.size()>{});
}

template <typename E, E V> inline constexpr auto stored_enum_name_array = enum_name_array<E, V>();

template <typename E, E V> consteval auto enum_name() -> std::string_view {
  constexpr auto &name_array = stored_enum_name_array<E, V>;
  return std::string_view{name_array.data(), name_array.size()};
}

template <typename E> constexpr auto enum_name(E value) -> std::string_view {
  if (auto idx = enum_index(value)) {
    constexpr std::size_t N = ENUM_RANGE_MAX - ENUM_RANGE_MIN + 1;
    constexpr auto names = []<std::size_t... I>(std::index_sequence<I...> /*indices*/) {
      return std::array<std::string_view, sizeof...(I)>{
          (enum_name<E, static_cast<E>(ENUM_RANGE_MIN + static_cast<int>(I))>())...};
    }(std::make_index_sequence<N>{});
    return names[*idx];
  }
  return {};
}

// Adapted from Boost.PFR: https://github.com/boostorg/pfr
template <std::size_t Index> using size_t_ = std::integral_constant<std::size_t, Index>;

template <typename T> consteval auto unsafe_declval() noexcept -> T {
  using func_ptr_t = T (*)();
  func_ptr_t ptr = nullptr;
  return ptr();
}

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-braces"
#pragma clang diagnostic ignored "-Wundefined-inline"
#pragma clang diagnostic ignored "-Wundefined-internal"
#pragma clang diagnostic ignored "-Wmissing-field-initializers"
#endif

///////////////////// Structure that can be converted to reference to anything
struct ubiq_lref_constructor {
  std::size_t ignore;
  template <typename Type> constexpr operator Type &() const && noexcept {
    return unsafe_declval<Type &>();
  }

  template <typename Type> constexpr operator Type &() const & noexcept {
    return unsafe_declval<Type &>();
  }
};

///////////////////// Structure that can be converted to rvalue reference to anything
struct ubiq_rref_constructor {
  std::size_t ignore;
  // Allows initialization of rvalue reference fields and move-only types
  template <typename Type> operator Type() const && noexcept { return unsafe_declval<Type>(); }
};

///////////////////// Helpers for initializable detection
// Note that these take O(N) compile time and memory!
template <typename T, std::size_t... I,
          typename /*Enable*/ = std::enable_if_t<std::is_copy_constructible_v<T>>>
consteval auto enable_if_initializable_helper(std::index_sequence<I...>) noexcept
    -> std::add_pointer_t<decltype(T{ubiq_lref_constructor{I}...})>;

template <typename T, std::size_t... I,
          typename /*Enable*/ = std::enable_if_t<!std::is_copy_constructible_v<T>>>
consteval auto enable_if_initializable_helper(std::index_sequence<I...>) noexcept
    // FIXME: Until 2025.7, clang and MSVC seems to have issues with this when counting
    // structs with a `vector<unique_ptr<T>>` field. Strangely, the issue only showcases
    // for C++ >= 20, and for C++ 17, everything works fine.
    // See: https://github.com/boostorg/pfr/issues/131
    // Waiting for either Boost.PFR or these compilers to be fixed — or until C++ 26 static
    // reflection is mature enough to use instead.
    -> std::add_pointer_t<decltype(T{ubiq_rref_constructor{I}...})>;

template <typename T, std::size_t N, typename U = std::size_t,
          typename /*Enable*/ =
              decltype(enable_if_initializable_helper<T>(std::make_index_sequence<N>()))>
using enable_if_initializable_helper_t = U;

template <typename T, std::size_t N>
consteval auto is_initializable(long /*unused*/) noexcept
    -> enable_if_initializable_helper_t<T, N, bool> {
  return true;
}
template <typename T, std::size_t N>
consteval auto is_initializable(int /*unused*/) noexcept -> bool {
  return false;
}

///////////////////// Helpers for range size detection
template <std::size_t Begin, std::size_t Last>
using is_one_element_range = std::integral_constant<bool, Begin == Last>;

using multi_element_range = std::false_type;
using one_element_range = std::true_type;

///////////////////// Fields count next expected compiler limitation
consteval auto fields_count_compiler_limitation_next(std::size_t n) noexcept -> std::size_t {
#if defined(_MSC_VER) && (_MSC_VER <= 1920)
  if (n < 1024)
    return 1024;
#else
  static_cast<void>(n);
#endif
  return std::numeric_limits<std::size_t>::max();
}

///////////////////// Fields count upper bound based on sizeof(T)
template <typename T> consteval auto fields_count_upper_bound_loose() noexcept -> std::size_t {
  return sizeof(T) * std::numeric_limits<unsigned char>::digits;
}

///////////////////// Fields count binary search.
// Template instantiation:
// depth is O(log(result)), count is O(log(result)), cost is O(result * log(result)).
template <typename T, std::size_t Begin, std::size_t Last>
consteval auto fields_count_binary_search(one_element_range /*range*/, long /*unused*/) noexcept
    -> std::size_t {
  static_assert(Begin == Last, "Internal logic error.");
  return Begin;
}
template <typename T, std::size_t Begin, std::size_t Last>
consteval auto fields_count_binary_search(multi_element_range /*range*/, int /*unused*/) noexcept
    -> std::size_t;
template <typename T, std::size_t Begin, std::size_t Last>
consteval auto fields_count_binary_search(multi_element_range /*range*/, long /*unused*/) noexcept
    -> enable_if_initializable_helper_t<T, (Begin + Last + 1) / 2> {
  constexpr std::size_t next_v = (Begin + Last + 1) / 2;
  return fields_count_binary_search<T, next_v, Last>(is_one_element_range<next_v, Last>{}, 1L);
}
template <typename T, std::size_t Begin, std::size_t Last>
consteval auto fields_count_binary_search(multi_element_range /*range*/, int /*unused*/) noexcept
    -> std::size_t {
  constexpr std::size_t next_v = (Begin + Last + 1) / 2 - 1;
  return fields_count_binary_search<T, Begin, next_v>(is_one_element_range<Begin, next_v>{}, 1L);
}

template <typename T, std::size_t Begin, std::size_t N>
consteval auto fields_count_upper_bound(int /*unused*/, int /*unused*/) noexcept -> std::size_t {
  return N - 1;
}
template <typename T, std::size_t Begin, std::size_t N>
  requires(N > fields_count_upper_bound_loose<T>())
consteval auto fields_count_upper_bound(long /*unused*/, long /*unused*/) noexcept -> std::size_t {
  return fields_count_upper_bound_loose<T>();
}
template <typename T, std::size_t Begin, std::size_t N>
consteval auto fields_count_upper_bound(long /*unused*/, int /*unused*/) noexcept
    -> enable_if_initializable_helper_t<T, N> {
  constexpr std::size_t next_optimal = Begin + (N - Begin) * 2;
  constexpr std::size_t next = std::min(next_optimal, fields_count_compiler_limitation_next(N));
  return fields_count_upper_bound<T, Begin, next>(1L, 1L);
}

///////////////////// Fields count lower bound linear search.
// Template instantiation: depth is O(log(result)), count is O(result), cost is O(result^2).
template <typename T, std::size_t Begin, std::size_t Last, typename RangeSize, std::size_t Result>
consteval auto fields_count_lower_bound(RangeSize /*size*/, size_t_<Result> /*result*/) noexcept
    -> std::size_t {
  return Result;
}
template <typename T, std::size_t Begin, std::size_t Last>
consteval auto fields_count_lower_bound(one_element_range /*range*/,
                                        size_t_<0> /*result*/ = {}) noexcept -> std::size_t {
  static_assert(Begin == Last, "Internal logic error.");
  return is_initializable<T, Begin>(1L) ? Begin : 0;
}
template <typename T, std::size_t Begin, std::size_t Last>
consteval auto fields_count_lower_bound(multi_element_range /*range*/,
                                        size_t_<0> /*result*/ = {}) noexcept -> std::size_t {
  // Binary partition to limit template depth.
  constexpr std::size_t middle = Begin + (Last - Begin) / 2;
  constexpr std::size_t result_maybe =
      fields_count_lower_bound<T, Begin, middle>(is_one_element_range<Begin, middle>{});
  return fields_count_lower_bound<T, middle + 1, Last>(is_one_element_range<middle + 1, Last>{},
                                                       size_t_<result_maybe>{});
}

template <typename T, std::size_t Begin, std::size_t Result>
consteval auto fields_count_lower_bound_unbounded(int /*unused*/,
                                                  size_t_<Result> /*result*/) noexcept
    -> std::size_t {
  return Result;
}
template <typename T, std::size_t Begin>
  requires(Begin >= fields_count_upper_bound_loose<T>())
consteval auto fields_count_lower_bound_unbounded(long /*unused*/, size_t_<0> /*result*/) noexcept
    -> std::size_t {
  return fields_count_upper_bound_loose<T>();
}
template <typename T, std::size_t Begin>
consteval auto fields_count_lower_bound_unbounded(int /*unused*/, size_t_<0> /*result*/) noexcept
    -> std::size_t {
  constexpr std::size_t last = std::min(Begin * 2, fields_count_upper_bound_loose<T>()) - 1;
  constexpr std::size_t result_maybe =
      fields_count_lower_bound<T, Begin, last>(is_one_element_range<Begin, last>{});
  return fields_count_lower_bound_unbounded<T, last + 1>(1L, size_t_<result_maybe>{});
}

///////////////////// Choosing between array size, unbounded binary search, and linear search
/// followed by unbounded binary search.
template <typename T>
  requires std::is_array_v<T>
consteval auto count_fields_dispatch(long /*unused*/, long /*unused*/) noexcept -> std::size_t {
  return sizeof(T) / sizeof(std::remove_all_extents_t<T>);
}
template <typename T>
consteval auto count_fields_dispatch(long /*unused*/, int /*unused*/) noexcept
    -> decltype(sizeof(T{})) {
  constexpr std::size_t typical_fields_count = 4;
  constexpr std::size_t last =
      fields_count_upper_bound<T, typical_fields_count / 2, typical_fields_count>(1L, 1L);
  return fields_count_binary_search<T, 0, last>(is_one_element_range<0, last>{}, 1L);
}
template <typename T>
consteval auto count_fields_dispatch(int /*unused*/, int /*unused*/) noexcept -> std::size_t {
  // T is not default aggregate initializable. This means that at least one of the members is not
  // default-constructible. Use linear search to find the smallest valid initializer, after which we
  // unbounded binary search for the largest.
  constexpr std::size_t begin = fields_count_lower_bound_unbounded<T, 1>(1L, size_t_<0>{});

  constexpr std::size_t last = fields_count_upper_bound<T, begin, begin + 1>(1L, 1L);
  return fields_count_binary_search<T, begin, last>(is_one_element_range<begin, last>{}, 1L);
}

template <typename T> consteval auto count_fields() noexcept -> std::size_t {
  return count_fields_dispatch<std::remove_cv_t<T>>(1L, 1L);
}

#ifdef __clang__
#pragma clang diagnostic pop
#endif

namespace sequence_tuple {

template <std::size_t N, typename T> struct base_from_member {
  T value; // NOLINT
};

template <typename I, typename... Tail> struct tuple_base;

template <std::size_t... I, typename... Tail>
// NOLINTNEXTLINE(cppcoreguidelines-special-member-functions)
struct tuple_base<std::index_sequence<I...>, Tail...> : base_from_member<I, Tail>... {
  static constexpr std::size_t size_v = sizeof...(I);

  // We do not use `noexcept` in the following functions, because if user forget to put one then
  // clang will issue an error: "error: exception specification of explicitly defaulted default
  // constructor does not match the calculated one".
  constexpr tuple_base() = default;
  constexpr tuple_base(tuple_base &&) = default;
  constexpr tuple_base(const tuple_base &) = default;

  constexpr tuple_base(Tail... v) noexcept : base_from_member<I, Tail>{v}... {}
};

template <> struct tuple_base<std::index_sequence<>> {
  static constexpr std::size_t size_v = 0;
};

template <std::size_t N, typename T>
constexpr auto get_impl(base_from_member<N, T> &t) noexcept -> T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <std::size_t N, typename T>
constexpr auto get_impl(const base_from_member<N, T> &t) noexcept -> const T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <std::size_t N, typename T>
constexpr auto get_impl(volatile base_from_member<N, T> &t) noexcept -> volatile T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <std::size_t N, typename T>
constexpr auto get_impl(const volatile base_from_member<N, T> &t) noexcept -> const volatile T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <std::size_t N, typename T>
// NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
constexpr auto get_impl(base_from_member<N, T> &&t) noexcept -> T && {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return std::forward<T>(t.value);
}

template <typename T, std::size_t N>
constexpr auto get_by_type_impl(base_from_member<N, T> &t) noexcept -> T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <typename T, std::size_t N>
constexpr auto get_by_type_impl(const base_from_member<N, T> &t) noexcept -> const T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <typename T, std::size_t N>
constexpr auto get_by_type_impl(volatile base_from_member<N, T> &t) noexcept -> volatile T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <typename T, std::size_t N>
constexpr auto get_by_type_impl(const volatile base_from_member<N, T> &t) noexcept -> const
    volatile T & {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return t.value;
}
template <typename T, std::size_t N>
// NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
constexpr auto get_by_type_impl(base_from_member<N, T> &&t) noexcept -> T && {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return std::forward<T>(t.value);
}
template <typename T, std::size_t N>
constexpr auto get_by_type_impl(const base_from_member<N, T> &&t) noexcept -> const T && {
  // NOLINTNEXTLINE(clang-analyzer-core.uninitialized.UndefReturn,clang-analyzer-core.CallAndMessage)
  return std::forward<T>(t.value);
}

template <typename... Values>
struct tuple : tuple_base<std::make_index_sequence<sizeof...(Values)>, Values...> {
  using tuple_base<std::make_index_sequence<sizeof...(Values)>, Values...>::tuple_base;

  constexpr static std::size_t size() noexcept { return sizeof...(Values); }
  constexpr static bool empty() noexcept { return size() == 0; }
};

template <std::size_t N, typename... T>
constexpr auto get(tuple<T...> &t) noexcept -> decltype(auto) {
  static_assert(N < tuple<T...>::size_v, "Tuple index out of bound");
  return sequence_tuple::get_impl<N>(t);
}
template <std::size_t N, typename... T>
constexpr auto get(const tuple<T...> &t) noexcept -> decltype(auto) {
  static_assert(N < tuple<T...>::size_v, "Tuple index out of bound");
  return sequence_tuple::get_impl<N>(t);
}
template <std::size_t N, typename... T>
constexpr auto get(const volatile tuple<T...> &t) noexcept -> decltype(auto) {
  static_assert(N < tuple<T...>::size_v, "Tuple index out of bound");
  return sequence_tuple::get_impl<N>(t);
}
template <std::size_t N, typename... T>
constexpr auto get(volatile tuple<T...> &t) noexcept -> decltype(auto) {
  static_assert(N < tuple<T...>::size_v, "Tuple index out of bound");
  return sequence_tuple::get_impl<N>(t);
}
template <std::size_t N, typename... T>
constexpr auto get(tuple<T...> &&t) noexcept -> decltype(auto) {
  static_assert(N < tuple<T...>::size_v, "Tuple index out of bound");
  return sequence_tuple::get_impl<N>(std::move(t));
}

template <std::size_t I, typename T>
using tuple_element = std::remove_reference<decltype(sequence_tuple::get<I>(std::declval<T>()))>;

template <typename... Args> constexpr auto make_sequence_tuple(Args... args) noexcept {
  return sequence_tuple::tuple<Args...>{args...};
}

} // namespace sequence_tuple

// NOLINTNEXTLINE(cppcoreguidelines-missing-std-forward)
template <typename... Args> constexpr auto make_tuple_of_references(Args &&...args) noexcept {
  return sequence_tuple::tuple<Args &...>{args...};
}

template <typename T>
constexpr auto tie_as_tuple(T & /*val*/, size_t_<0> /*size*/ /*size*/) noexcept {
  return sequence_tuple::tuple<>{};
}
template <typename T>
constexpr auto tie_as_tuple(
    T &val, size_t_<1> /*size*/ /*size*/,
    std::enable_if_t<std::is_class_v<std::remove_cv_t<T>>> * /*unused*/ = nullptr) noexcept {
  auto &[a] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a);
}
template <typename T>
constexpr auto tie_as_tuple(
    T &val, size_t_<1> /*size*/ /*size*/,
    std::enable_if_t<!std::is_class_v<std::remove_cv_t<T>>> * /*unused*/ = nullptr) noexcept {
  return make_tuple_of_references(val);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<2> /*size*/ /*size*/) noexcept {
  auto &[a, b] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<3> /*size*/ /*size*/) noexcept {
  auto &[a, b, c] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<4> /*size*/ /*size*/) noexcept {
  auto &[a, b, c, d] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<5> /*size*/ /*size*/) noexcept {
  auto &[a, b, c, d, e] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<6> /*size*/ /*size*/) noexcept {
  auto &[a, b, c, d, e, f] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<7> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<8> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<9> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<10> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<11> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<12> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<13> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<14> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<15> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<16> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<17> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<18> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<19> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<20> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<21> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<22> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<23> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<24> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<25> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<26> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<27> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<28> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<29> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<30> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<31> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<32> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<33> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<34> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<35> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<36> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<37> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<38> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<39> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<40> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<41> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<42> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<43> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<44> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<45> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<46> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<47> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<48> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<49> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<50> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<51> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<52> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<53> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<54> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<55> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<56> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<57> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj,
         ak] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<58> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<59> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<60> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<61> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<62> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<63> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<64> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<65> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<66> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<67> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<68> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<69> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<70> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<71> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<72> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<73> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<74> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<75> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<76> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<77> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<78> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<79> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<80> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH,
         aJ] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<81> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<82> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<83> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<84> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<85> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<86> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<87> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<88> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<89> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<90> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<91> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<92> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<93> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<94> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<95> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<96> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<97> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<98> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<99> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<100> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<101> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<102> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<103> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh,
         bj] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<104> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<105> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY,
                                  aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk, bl);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<106> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY,
                                  aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk, bl, bm);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<107> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY,
                                  aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk, bl, bm, bn);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<108> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY,
                                  aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk, bl, bm, bn, bp);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<109> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x,
                                  y, z, A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, U, V, W,
                                  X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al, am, an, ap,
                                  aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF,
                                  aG, aH, aJ, aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY,
                                  aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk, bl, bm, bn, bp, bq);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<110> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<111> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<112> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<113> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<114> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<115> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<116> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<117> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<118> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<119> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<120> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<121> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<122> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<123> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<124> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<125> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<126> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG,
         bH] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<127> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<128> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<129> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<130> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<131> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<132> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<133> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<134> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<135> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<136> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<137> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<138> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<139> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<140> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<141> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<142> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<143> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<144> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<145> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<146> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<147> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<148> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<149> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg,
         ch] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<150> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<151> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<152> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<153> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<154> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<155> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<156> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<157> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<158> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<159> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<160> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<161> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<162> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<163> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<164> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<165> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<166> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<167> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<168> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<169> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<170> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<171> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<172> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF,
         cG] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<173> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<174> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<175> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<176> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<177> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<178> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<179> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<180> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<181> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<182> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<183> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<184> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<185> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<186> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<187> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<188> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<189> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<190> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<191> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<192> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<193> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<194> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df] =
      const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<195> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df,
         dg] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<196> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg,
         dh] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg, dh);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<197> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg,
         dh, dj] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg, dh,
      dj);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<198> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg,
         dh, dj, dk] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg, dh,
      dj, dk);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<199> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg,
         dh, dj, dk, dl] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg, dh,
      dj, dk, dl);
}
template <typename T> constexpr auto tie_as_tuple(T &val, size_t_<200> /*size*/) noexcept {
  auto &[a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F,
         G, H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak,
         al, am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ,
         aK, aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj,
         bk, bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH,
         bJ, bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch,
         cj, ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG,
         cH, cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg,
         dh, dj, dk, dl, dm] = const_cast<std::remove_cv_t<T> &>(val);
  return make_tuple_of_references(
      a, b, c, d, e, f, g, h, j, k, l, m, n, p, q, r, s, t, u, v, w, x, y, z, A, B, C, D, E, F, G,
      H, J, K, L, M, N, P, Q, R, S, U, V, W, X, Y, Z, aa, ab, ac, ad, ae, af, ag, ah, aj, ak, al,
      am, an, ap, aq, ar, as, at, au, av, aw, ax, ay, az, aA, aB, aC, aD, aE, aF, aG, aH, aJ, aK,
      aL, aM, aN, aP, aQ, aR, aS, aU, aV, aW, aX, aY, aZ, ba, bb, bc, bd, be, bf, bg, bh, bj, bk,
      bl, bm, bn, bp, bq, br, bs, bt, bu, bv, bw, bx, by, bz, bA, bB, bC, bD, bE, bF, bG, bH, bJ,
      bK, bL, bM, bN, bP, bQ, bR, bS, bU, bV, bW, bX, bY, bZ, ca, cb, cc, cd, ce, cf, cg, ch, cj,
      ck, cl, cm, cn, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz, cA, cB, cC, cD, cE, cF, cG, cH,
      cJ, cK, cL, cM, cN, cP, cQ, cR, cS, cU, cV, cW, cX, cY, cZ, da, db, dc, dd, de, df, dg, dh,
      dj, dk, dl, dm);
}

template <typename T> constexpr auto tie_as_tuple(T &val) noexcept {
  return tie_as_tuple(val, size_t_<count_fields<T>()>{});
}

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundefined-internal"
#pragma clang diagnostic ignored "-Wundefined-var-template"
#endif

// This typename has external linkage while T has not sure.
template <typename T> struct wrapper {
  // NOLINTNEXTLINE(cppcoreguidelines-avoid-const-or-ref-data-members)
  const T value;
};

// This variable servers as a link-time assert.
// If linker requires it, then `fake_object()` is used at runtime.
template <typename T> extern const wrapper<T> DO_NOT_USE_PFR_WITH_LOCAL_TYPES;

// For returning non default constructible types, it's exclusively used in member name retrieval.
template <typename T> constexpr auto fake_object() noexcept -> const T & {
  return DO_NOT_USE_PFR_WITH_LOCAL_TYPES<T>.value;
}

#ifdef __clang__
#pragma clang diagnostic pop
#endif

template <auto ptr> consteval auto field_name_array() noexcept {
#if defined(__clang__)
  constexpr std::string_view s = __PRETTY_FUNCTION__;
  constexpr std::size_t start = s.rfind(".value.") + 7;
  constexpr std::size_t end = s.rfind("}]");
#elif defined(__GNUC__)
  constexpr std::string_view s = __PRETTY_FUNCTION__;
  constexpr std::size_t start = s.rfind("::") + 2;
  constexpr std::size_t end = s.rfind(")]");
#elif defined(_MSC_VER)
  constexpr std::string_view s = __FUNCSIG__;
  constexpr std::size_t start = s.rfind("->value->") + 9;
  constexpr std::size_t end = s.rfind(">(void)");
#else
#error Unsupported compiler
#endif
  constexpr std::string_view name = s.substr(start, end - start);
  return [&name]<std::size_t... I>(std::index_sequence<I...> /*indices*/) {
    return std::array{name[I]...};
  }(std::make_index_sequence<name.size()>{});
}

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wundefined-var-template"

// clang 16 and earlier don't support address of non-static member as template parameter
// but fortunately it's possible to use C++20 non-type template parameters in another way
// even in clang 16 and more older clangs
// all we need is to wrap pointer into 'clang_wrapper_t' and then pass it into template
template <typename T> struct clang_wrapper_t {
  T v;
};
template <typename T> clang_wrapper_t(T) -> clang_wrapper_t<T>;

template <typename T> constexpr auto make_clang_wrapper(const T &arg) noexcept {
  return clang_wrapper_t{arg};
}

#else

template <typename T> constexpr const T &make_clang_wrapper(const T &arg) noexcept {
  // It's everything OK with address of non-static member as template parameter support on this
  // compiler so we don't need a wrapper here, just pass the pointer into template
  return arg;
}

#endif

template <typename T, std::size_t I>
inline constexpr auto stored_field_name_array = field_name_array<make_clang_wrapper(
    std::addressof(sequence_tuple::get<I>(tie_as_tuple(fake_object<T>()))))>();

template <typename T, std::size_t I> consteval auto field_name() noexcept -> std::string_view {
  constexpr auto &name_array = stored_field_name_array<T, I>;
  return std::string_view{name_array.data(), name_array.size()};
}

} // namespace detail

enum class color : std::uint8_t {
  /* Modifiers */
  bold,
  dim,
  reset,
  /* Colors */
  black,
  blue,
  cyan,
  gray,
  green,
  magenta,
  red,
  white,
  yellow,
};

struct styles {
  // NOLINTBEGIN(misc-non-private-member-variables-in-classes)
  color string = color::green;
  color character = color::green;
  color number = color::yellow;
  color boolean = color::yellow;
  color enumeration = color::cyan;
  color null = color::bold;
  color special = color::cyan;
  // NOLINTEND(misc-non-private-member-variables-in-classes)

  [[nodiscard]] auto operator==(const styles &other) const -> bool = default;
  [[nodiscard]] auto operator!=(const styles &other) const -> bool { return !(*this == other); }
  [[nodiscard]] auto operator<=>(const styles &other) const -> std::strong_ordering = default;
};

namespace detail {

struct c {
  /* Modifiers */
  std::function<std::string(std::string_view)> bold;
  std::function<std::string(std::string_view)> dim;
  std::function<std::string(std::string_view)> reset;
  /* Colors */
  std::function<std::string(std::string_view)> black;
  std::function<std::string(std::string_view)> blue;
  std::function<std::string(std::string_view)> cyan;
  std::function<std::string(std::string_view)> gray;
  std::function<std::string(std::string_view)> green;
  std::function<std::string(std::string_view)> magenta;
  std::function<std::string(std::string_view)> red;
  std::function<std::string(std::string_view)> white;
  std::function<std::string(std::string_view)> yellow;

  /* Styles */
  std::function<std::string(std::string_view)> string;
  std::function<std::string(std::string_view)> character;
  std::function<std::string(std::string_view)> number;
  std::function<std::string(std::string_view)> boolean;
  std::function<std::string(std::string_view)> enumeration;
  std::function<std::string(std::string_view)> null;
  std::function<std::string(std::string_view)> special;
};

[[nodiscard]] inline auto colorize(std::string_view s, color color) -> std::string {
  switch (color) {
  case color::bold:
    return "\033[1m" + std::string(s) + "\033[22m";
  case color::dim:
    return "\033[2m" + std::string(s) + "\033[22m";
  case color::reset:
    return "\033[0m" + std::string(s);

  /* Colors */
  case color::black:
    return "\033[30m" + std::string(s) + "\033[39m";
  case color::blue:
    return "\033[34m" + std::string(s) + "\033[39m";
  case color::cyan:
    return "\033[36m" + std::string(s) + "\033[39m";
  case color::gray:
    return "\033[90m" + std::string(s) + "\033[39m";
  case color::green:
    return "\033[32m" + std::string(s) + "\033[39m";
  case color::magenta:
    return "\033[35m" + std::string(s) + "\033[39m";
  case color::red:
    return "\033[31m" + std::string(s) + "\033[39m";
  case color::white:
    return "\033[37m" + std::string(s) + "\033[39m";
  case color::yellow:
    return "\033[33m" + std::string(s) + "\033[39m";
  }

  return std::string(s); // Fallback to no color
}

[[nodiscard]] inline auto build_c(bool color_enabled, mp::styles styles) -> c {
  c c;

  if (color_enabled) {
    c.bold = [](std::string_view s) { return colorize(s, color::bold); };
    c.dim = [](std::string_view s) { return colorize(s, color::dim); };
    c.reset = [](std::string_view s) { return colorize(s, color::reset); };
    c.black = [](std::string_view s) { return colorize(s, color::black); };
    c.blue = [](std::string_view s) { return colorize(s, color::blue); };
    c.cyan = [](std::string_view s) { return colorize(s, color::cyan); };
    c.gray = [](std::string_view s) { return colorize(s, color::gray); };
    c.green = [](std::string_view s) { return colorize(s, color::green); };
    c.magenta = [](std::string_view s) { return colorize(s, color::magenta); };
    c.red = [](std::string_view s) { return colorize(s, color::red); };
    c.white = [](std::string_view s) { return colorize(s, color::white); };
    c.yellow = [](std::string_view s) { return colorize(s, color::yellow); };

    c.string = [styles](std::string_view s) { return colorize(s, styles.string); };
    c.character = [styles](std::string_view s) { return colorize(s, styles.character); };
    c.number = [styles](std::string_view s) { return colorize(s, styles.number); };
    c.boolean = [styles](std::string_view s) { return colorize(s, styles.boolean); };
    c.enumeration = [styles](std::string_view s) { return colorize(s, styles.enumeration); };
    c.null = [styles](std::string_view s) { return colorize(s, styles.null); };
    c.special = [styles](std::string_view s) { return colorize(s, styles.special); };
  } else {
    c.bold = [](std::string_view s) { return std::string(s); };
    c.dim = [](std::string_view s) { return std::string(s); };
    c.reset = [](std::string_view s) { return std::string(s); };
    c.black = [](std::string_view s) { return std::string(s); };
    c.blue = [](std::string_view s) { return std::string(s); };
    c.cyan = [](std::string_view s) { return std::string(s); };
    c.gray = [](std::string_view s) { return std::string(s); };
    c.green = [](std::string_view s) { return std::string(s); };
    c.magenta = [](std::string_view s) { return std::string(s); };
    c.red = [](std::string_view s) { return std::string(s); };
    c.white = [](std::string_view s) { return std::string(s); };
    c.yellow = [](std::string_view s) { return std::string(s); };

    c.string = [](std::string_view s) { return std::string(s); };
    c.character = [](std::string_view s) { return std::string(s); };
    c.number = [](std::string_view s) { return std::string(s); };
    c.boolean = [](std::string_view s) { return std::string(s); };
    c.enumeration = [](std::string_view s) { return std::string(s); };
    c.null = [](std::string_view s) { return std::string(s); };
    c.special = [](std::string_view s) { return std::string(s); };
  }
  return c;
}

} // namespace detail

namespace node {

using ref = std::pair<std::string, std::uintptr_t>;

struct ref_hash {
  [[nodiscard]] auto operator()(const ref &ref) const noexcept -> std::size_t {
    const std::size_t h1 = std::hash<std::string>{}(ref.first);
    const std::size_t h2 = std::hash<std::uintptr_t>{}(ref.second);
    return h1 ^ (h2 << 1);
  }
};

struct circular_node;
struct text_node;
struct inline_wrap_node;
struct sequence_node;
struct between_node;

using node = std::variant<circular_node, text_node, inline_wrap_node, sequence_node, between_node>;

struct circular_node {
  mp::node::ref ref = {"", 0};
};

struct text_node {
  std::string value;
  mp::node::ref ref = {"", 0};
};

struct inline_wrap_node {
  std::unique_ptr<node> inline_node;
  std::unique_ptr<node> wrap_node;
  mp::node::ref ref = {"", 0};
};

struct sequence_node {
  std::vector<std::unique_ptr<node>> values;
  mp::node::ref ref = {"", 0};
};

struct between_node {
  std::vector<std::unique_ptr<node>> values;
  std::unique_ptr<node> open;
  std::unique_ptr<node> close;
  mp::node::ref ref = {"", 0};
};

template <typename T> [[nodiscard]] inline auto circular(const T &value) -> std::unique_ptr<node> {
  return std::make_unique<node>(
      circular_node{.ref = {std::string(detail::type_name<std::remove_cvref_t<T>>()),
                            reinterpret_cast<std::uintptr_t>(std::addressof(value))}});
}

[[nodiscard]] inline auto text(std::string_view value) -> std::unique_ptr<node> {
  return std::make_unique<node>(text_node{.value = std::string(value)});
}

[[nodiscard]] inline auto inline_wrap(std::unique_ptr<node> inline_node,
                                      std::unique_ptr<node> wrap_node) -> std::unique_ptr<node> {
  return std::make_unique<node>(inline_wrap_node{
      .inline_node = std::move(inline_node),
      .wrap_node = std::move(wrap_node),
  });
}

[[nodiscard]] inline auto sequence(std::vector<std::unique_ptr<node>> &&values)
    -> std::unique_ptr<node> {
  return std::make_unique<node>(sequence_node{.values = std::move(values)});
}

template <typename... Ns>
  requires(std::same_as<std::decay_t<Ns>, std::unique_ptr<node>> && ...)
[[nodiscard]] inline auto sequence(Ns &&...nodes) -> std::unique_ptr<node> {
  std::vector<std::unique_ptr<node>> vals;
  vals.reserve(sizeof...(Ns));
  (vals.push_back(std::forward<Ns>(nodes)), ...);
  return std::make_unique<node>(sequence_node{.values = std::move(vals)});
}

[[nodiscard]] inline auto between(std::vector<std::unique_ptr<node>> &&values,
                                  std::unique_ptr<node> open = nullptr,
                                  std::unique_ptr<node> close = nullptr) -> std::unique_ptr<node> {
  return std::make_unique<node>(between_node{
      .values = std::move(values),
      .open = std::move(open),
      .close = std::move(close),
  });
}

[[nodiscard]] inline auto clone_node(const node &node) -> std::unique_ptr<mp::node::node> {
  return std::visit(
      [](auto &&node) -> std::unique_ptr<mp::node::node> {
        using V = std::decay_t<decltype(node)>;
        if constexpr (std::same_as<V, mp::node::text_node> ||
                      std::same_as<V, mp::node::circular_node>) {
          return std::make_unique<mp::node::node>(node);
        } else if constexpr (std::same_as<V, mp::node::inline_wrap_node>) {
          return std::make_unique<mp::node::node>(
              mp::node::inline_wrap_node{.inline_node = clone_node(*node.inline_node),
                                         .wrap_node = clone_node(*node.wrap_node),
                                         .ref = node.ref});
        } else if constexpr (std::same_as<V, mp::node::sequence_node>) {
          std::vector<std::unique_ptr<mp::node::node>> values;
          values.reserve(node.values.size());
          for (auto &value : node.values)
            values.push_back(clone_node(*value));
          return std::make_unique<mp::node::node>(
              mp::node::sequence_node{std::move(values), node.ref});
        } else {
          std::vector<std::unique_ptr<mp::node::node>> values;
          values.reserve(node.values.size());
          for (auto &value : node.values)
            values.push_back(clone_node(*value));
          return std::make_unique<mp::node::node>(
              mp::node::between_node{.values = std::move(values),
                                     .open = node.open ? clone_node(*node.open) : nullptr,
                                     .close = node.close ? clone_node(*node.close) : nullptr,
                                     .ref = node.ref});
        }
      },
      node);
}

} // namespace node

namespace detail {

enum class option_tag : std::uint8_t {
  depth,
  indent,
  break_length,
  sorted,
  omitted_fields,
  numeric_separator,
  trailing_comma,
  sequence_bracket_spacing,
  object_curly_spacing,
  reference_pointer,
  max_sequence_length,
  max_string_length,
  colors,
  styles,

  /* Internal */
  level,
  ancestors,
};

// Settings definition inspired by indicators:
// https://github.com/p-ranav/indicators/blob/3872f37abd90d7557bac5f834bfb45bd6c75259a/include/indicators/setting.hpp
template <typename T, option_tag Tag> struct setting {
  template <typename... Args>
  explicit setting(Args &&...args)
    requires std::is_constructible_v<T, Args...>
      : value(std::forward<Args>(args)...) {}
  static constexpr auto tag = Tag;
  using type = T;

  T value{};
};

template <typename T, typename... Os>
  requires(std::is_base_of_v<setting<typename Os::type, Os::tag>, Os> && ...)
inline void patch_options(T &options, Os &&...opts) {
  constexpr std::size_t fields_count = count_fields<T>();
  const auto fields = tie_as_tuple(options, size_t_<fields_count>{});

  // NOLINTNEXTLINE(cppcoreguidelines-missing-std-forward)
  auto set_option = [&fields]<typename O>(O &&opt) {
    auto try_set_field = [&fields,
                          &opt]<std::size_t I>(std::integral_constant<std::size_t, I> /*index*/) {
      if constexpr (field_name<T, I>() == enum_name<option_tag, O::tag>())
        sequence_tuple::get<I>(fields) = opt.value;
    };

    [&try_set_field]<std::size_t... I>(std::index_sequence<I...>) {
      (try_set_field(std::integral_constant<std::size_t, I>{}), ...);
    }(std::make_index_sequence<fields_count>{});
  };

  (set_option(std::forward<Os>(opts)), ...);
}

} // namespace detail

namespace option {

/**
 * @brief The maximum depth to inspect.
 */
using depth = detail::setting<std::size_t, detail::option_tag::depth>;

/**
 * @brief The number of spaces to use for indentation.
 */
using indent = detail::setting<std::size_t, detail::option_tag::indent>;

/**
 * @brief The maximum line length before breaking. If `indent` is `0`, this option is ignored.
 */
using break_length = detail::setting<std::size_t, detail::option_tag::break_length>;

/**
 * @brief Whether to sort the fields of objects (including std::unordered_(multi)map and
 * std::unordered_(multi)set) in the resulting string.
 */
using sorted = detail::setting<bool, detail::option_tag::sorted>;

/**
 * @brief A set of fields to omit from the output.
 *
 * NOTE: This option is not recursive and only omits the top-level fields.
 */
using omitted_fields =
    detail::setting<std::unordered_set<std::string>, detail::option_tag::omitted_fields>;

/**
 * @brief Whether to add separators as thousands separators in numbers. If not set to `false`,
 * it will use the provided string as the separator, e.g., `","` or `"_"`. If set to `true`,
 * the separator defaults to `","`.
 */
using numeric_separator =
    detail::setting<std::variant<bool, std::string_view>, detail::option_tag::numeric_separator>;

enum class trailing_comma_type : std::uint8_t {
  none,   // No trailing comma
  always, // Always add a trailing comma
  // NOLINTNEXTLINE(readability-identifier-naming)
  auto_, // Add a trailing comma if the last item is on a separate line
};

/**
 * @brief Whether to add a trailing comma to the last item in a vector or object.
 * If set to `auto_`, it will add a trailing comma if the last item is on a separate line.
 */
using trailing_comma = detail::setting<trailing_comma_type, detail::option_tag::trailing_comma>;

/**
 * @brief Whether to add spaces inside sequence brackets.
 */
using sequence_bracket_spacing =
    detail::setting<bool, detail::option_tag::sequence_bracket_spacing>;

/**
 * @brief Whether to add spaces inside object curly braces.
 */
using object_curly_spacing = detail::setting<bool, detail::option_tag::object_curly_spacing>;

/**
 * @brief Whether to add a reference pointer to circular references.
 * If set to `false`, circular references are displayed as `[Circular]`.
 */
using reference_pointer = detail::setting<bool, detail::option_tag::reference_pointer>;

/**
 * @brief The maximum length of a sequence to show. If the sequence is longer than this length,
 * it will be truncated and an ellipsis with the length of the truncation
 * (e.g., `[1, 2, ... 3 more items]`) will be shown.
 */
using max_sequence_length = detail::setting<std::size_t, detail::option_tag::max_sequence_length>;

/**
 * @brief The maximum length of a string to show. If the string is longer than this length,
 * it will be truncated and an ellipsis with the length of the truncation
 * (e.g., `"aa"... 3 more characters`) will be shown.
 */
using max_string_length = detail::setting<std::size_t, detail::option_tag::max_string_length>;

/**
 * @brief Whether to colorize the output with ANSI codes.
 */
using colors = detail::setting<bool, detail::option_tag::colors>;

/**
 * @brief Colors for different types of values.
 */
using styles = detail::setting<mp::styles, detail::option_tag::styles>;

/**
 * @brief Current level of inspection.
 *
 * NOTE: This option is only used internally to track the depth of inspection.
 */
using level = detail::setting<std::size_t, detail::option_tag::level>;
/**
 * @brief List of ancestors to avoid circular references.
 *
 * NOTE: This option is only used internally to track the ancestors of the current value.
 */
using ancestors = detail::setting<std::deque<node::ref>, detail::option_tag::ancestors>;

} // namespace option

namespace detail {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cert-err58-cpp)
inline std::unordered_map<std::string, std::any> default_options = []() {
  std::unordered_map<std::string, std::any> options;

  // NOLINTNEXTLINE(cppcoreguidelines-missing-std-forward)
  auto set_option = [&options]<typename O>(O &&option) {
    options[std::string(enum_name<detail::option_tag, O::tag>())] = option.value;
  };

  set_option(option::colors{detail::supports_ansi()});
  set_option(option::indent{2});
  set_option(option::depth{4});

  return options;
}();

template <typename T> inline void patch_options_with_global_default_options(T &options) {
  constexpr std::size_t fields_count = count_fields<T>();
  const auto fields = tie_as_tuple(options, size_t_<fields_count>{});

  auto try_set_field = [&fields]<std::size_t I>(std::integral_constant<std::size_t, I> /*index*/) {
    if (const auto it = default_options.find(std::string(field_name<T, I>()));
        it != default_options.end()) {
      auto &field = sequence_tuple::get<I>(fields);
      field = std::any_cast<std::remove_cvref_t<decltype(field)>>(it->second);
      return;
    }
  };

  [&try_set_field]<std::size_t... I>(std::index_sequence<I...>) {
    (try_set_field(std::integral_constant<std::size_t, I>{}), ...);
  }(std::make_index_sequence<fields_count>{});
}

} // namespace detail

template <typename... Os>
  requires(std::is_base_of_v<detail::setting<typename Os::type, Os::tag>, Os> && ...)
inline void set_options(Os &&...options) {
  // NOLINTNEXTLINE(cppcoreguidelines-missing-std-forward)
  auto set_option = []<typename O>(O &&option) {
    detail::default_options[std::string(detail::enum_name<detail::option_tag, O::tag>())] =
        option.value;
  };
  (set_option(std::forward<Os>(options)), ...);
}

struct inspect_options {
  /**
   * @brief The maximum depth to show.
   */
  std::size_t depth = std::numeric_limits<std::size_t>::max();
  /**
   * @brief The number of spaces to use for indentation.
   */
  std::size_t indent = 0;
  /**
   * @brief The maximum line length before breaking. If `indent` is `0`, this option is ignored.
   */
  std::size_t break_length = 80;
  /**
   * @brief Whether to sort the fields of objects (including std::unordered_(multi)map and
   * std::unordered_(multi)set) in the resulting string.
   */
  bool sorted = false;
  /**
   * @brief A set of fields to omit from the output.
   *
   * NOTE: This option is not recursive and only omits the top-level fields.
   */
  std::unordered_set<std::string> omitted_fields;
  /**
   * @brief Whether to add separators as thousands separators in numbers. If not set to `false`,
   * it will use the provided string as the separator, e.g., `","` or `"_"`. If set to `true`,
   * the separator defaults to `","`.
   */
  std::variant<bool, std::string_view> numeric_separator = false;
  /**
   * @brief Whether to add a trailing comma to the last item in a vector or object.
   * If set to `auto_`, it will add a trailing comma if the last item is on a separate line.
   */
  option::trailing_comma_type trailing_comma = option::trailing_comma_type::none;
  /**
   * @brief Whether to add spaces inside sequence brackets.
   */
  bool sequence_bracket_spacing = false;
  /**
   * @brief Whether to add spaces inside object curly braces.
   */
  bool object_curly_spacing = true;
  /**
   * @brief Whether to add a reference pointer to circular references.
   * If set to `false`, circular references are displayed as `[Circular]`.
   */
  bool reference_pointer = true;
  /**
   * @brief The maximum length of a sequence to show. If the sequence is longer than this length,
   * it will be truncated and an ellipsis with the length of the truncation
   * (e.g., `[1, 2, ... 3 more items]`) will be shown.
   */
  std::size_t max_sequence_length = std::numeric_limits<std::size_t>::max();
  /**
   * @brief The maximum length of a string to show. If the string is longer than this length,
   * it will be truncated and an ellipsis with the length of the truncation
   * (e.g., `"aa"... 3 more characters`) will be shown.
   */
  std::size_t max_string_length = std::numeric_limits<std::size_t>::max();
  /**
   * @brief Whether to colorize the output with ANSI codes.
   */
  bool colors = false;
  /**
   * @brief Colors for different types of values.
   */
  mp::styles styles;
};

struct inspect_method_options {
  /**
   * @brief The maximum depth to show.
   */
  std::size_t depth = std::numeric_limits<std::size_t>::max();
  /**
   * @brief Whether to sort the fields of objects (including std::unordered_(multi)map and
   * std::unordered_(multi)set) in the resulting string.
   */
  bool sorted = false;
  /**
   * @brief A set of fields to omit from the output.
   *
   * NOTE: This option is not recursive and only omits the top-level fields.
   */
  std::unordered_set<std::string> omitted_fields;
  /**
   * @brief Whether to add separators as thousands separators in numbers. If not set to `false`,
   * it will use the provided string as the separator, e.g., `","` or `"_"`. If set to `true`,
   * the separator defaults to `","`.
   */
  std::variant<bool, std::string_view> numeric_separator = false;
  /**
   * @brief Whether to add a trailing comma to the last item in a vector or object.
   * If set to `auto_`, it will add a trailing comma if the last item is on a separate line.
   */
  option::trailing_comma_type trailing_comma = option::trailing_comma_type::none;
  /**
   * @brief Whether to add spaces inside sequence brackets.
   */
  bool sequence_bracket_spacing = false;
  /**
   * @brief Whether to add spaces inside object curly braces.
   */
  bool object_curly_spacing = true;
  /**
   * @brief Whether to add a reference pointer to circular references.
   * If set to `false`, circular references are displayed as `[Circular]`.
   */
  bool reference_pointer = true;
  /**
   * @brief The maximum length of a sequence to show. If the sequence is longer than this length,
   * it will be truncated and an ellipsis with the length of the truncation
   * (e.g., `[1, 2, ... 3 more items]`) will be shown.
   */
  std::size_t max_sequence_length = std::numeric_limits<std::size_t>::max();
  /**
   * @brief The maximum length of a string to show. If the string is longer than this length,
   * it will be truncated and an ellipsis with the length of the truncation
   * (e.g., `"aa"... 3 more characters`) will be shown.
   */
  std::size_t max_string_length = std::numeric_limits<std::size_t>::max();
  /**
   * @brief Whether to colorize the output with ANSI codes.
   */
  bool colors = false;
  /**
   * @brief Colors for different types of values.
   */
  mp::styles styles;

  /* InspectOptions specific fields */
  /**
   * @brief Current level of inspection.
   */
  std::size_t level = 0;
  /**
   * @brief List of ancestors to avoid circular references.
   */
  std::deque<node::ref> ancestors;
  /**
   * @brief An struct that contains color functions.
   */
  detail::c c;
};

template <typename T>
[[nodiscard]] inline auto inspect_fallback(T &&value, const inspect_method_options &options)
    -> std::string {
  std::ostringstream oss;
  oss << std::hex << std::addressof(std::forward<T>(value));
  std::string address = oss.str();
  std::transform(address.begin(), address.end(), address.begin(),
                 [](auto c) { return std::tolower(c); });
  return options.c.special("[" + std::string(detail::type_name<std::remove_cvref_t<T>>()) + "@" +
                           address + "]");
}

namespace detail {

inline auto utf16_to_utf8(std::u16string_view sv) -> std::string {
  std::string out;
  out.reserve(sv.size());
  for (char32_t cp : sv) {
    if (cp <= 0x7F)
      out.push_back(static_cast<char>(cp));
    else if (cp <= 0x7ff) {
      out.push_back(static_cast<char>(0xc0 | ((cp >> 6) & 0x1f)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
    } else {
      out.push_back(static_cast<char>(0xe0 | ((cp >> 12) & 0x0f)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3f)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
    }
  }
  return out;
}

inline auto utf32_to_utf8(std::u32string_view sv) -> std::string {
  std::string out;
  out.reserve(sv.size());
  for (char32_t cp : sv) {
    if (cp <= 0x7F)
      out.push_back(static_cast<char>(cp));
    else if (cp <= 0x7ff) {
      out.push_back(static_cast<char>(0xc0 | ((cp >> 6) & 0x1f)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
    } else if (cp <= 0xffff) {
      out.push_back(static_cast<char>(0xe0 | ((cp >> 12) & 0x0f)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3f)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else {
      out.push_back(static_cast<char>(0xf0 | ((cp >> 18) & 0x07)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3f)));
      out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3f)));
      out.push_back(static_cast<char>(0x80 | (cp & 0x3f)));
    }
  }
  return out;
}

template <typename S>
concept string_type =
    std::same_as<std::decay_t<S>, const char *> || std::same_as<std::decay_t<S>, char *> ||
    std::same_as<std::decay_t<S>, const signed char *> ||
    std::same_as<std::decay_t<S>, signed char *> ||
    std::same_as<std::decay_t<S>, const unsigned char *> ||
    std::same_as<std::decay_t<S>, unsigned char *> ||
    std::same_as<std::decay_t<S>, const wchar_t *> || std::same_as<std::decay_t<S>, wchar_t *> ||
    std::same_as<std::decay_t<S>, const char8_t *> || std::same_as<std::decay_t<S>, char8_t *> ||
    std::same_as<std::decay_t<S>, const char16_t *> || std::same_as<std::decay_t<S>, char16_t *> ||
    std::same_as<std::decay_t<S>, const char32_t *> || std::same_as<std::decay_t<S>, char32_t *> ||
    std::same_as<std::decay_t<S>, std::string> || std::same_as<std::decay_t<S>, std::wstring> ||
    std::same_as<std::decay_t<S>, std::u8string> || std::same_as<std::decay_t<S>, std::u16string> ||
    std::same_as<std::decay_t<S>, std::u32string> ||
    std::same_as<std::decay_t<S>, std::string_view> ||
    std::same_as<std::decay_t<S>, std::wstring_view> ||
    std::same_as<std::decay_t<S>, std::u8string_view> ||
    std::same_as<std::decay_t<S>, std::u16string_view> ||
    std::same_as<std::decay_t<S>, std::u32string_view> ||
    std::same_as<std::decay_t<S>, std::pmr::string> ||
    std::same_as<std::decay_t<S>, std::pmr::wstring> ||
    std::same_as<std::decay_t<S>, std::pmr::u8string> ||
    std::same_as<std::decay_t<S>, std::pmr::u16string> ||
    std::same_as<std::decay_t<S>, std::pmr::u32string>;

template <string_type S> [[nodiscard]] auto extract_string_content(S &&s) -> std::string {
  using T = std::decay_t<S>;

  // Raw char pointers (8-bit)
  if constexpr (std::same_as<T, const char *> || std::same_as<T, char *> ||
                std::same_as<T, const signed char *> || std::same_as<T, signed char *> ||
                std::same_as<T, const unsigned char *> || std::same_as<T, unsigned char *> ||
                std::same_as<T, const char8_t *> || std::same_as<T, char8_t *>) {
    return s ? std::string(reinterpret_cast<const char *>(s)) : "";
  }
  // wchar_t* → UTF-8
  else if constexpr (std::same_as<T, const wchar_t *> || std::same_as<T, wchar_t *>) {
    if (!s)
      return "";
    // Assume wchar_t is UTF-32 on Linux, UTF-16 on Windows
    if constexpr (sizeof(wchar_t) == 2) {
      return utf16_to_utf8(std::u16string_view(reinterpret_cast<const char16_t *>(s),
                                               std::char_traits<wchar_t>::length(s)));
    } else {
      return utf32_to_utf8(std::u32string_view(reinterpret_cast<const char32_t *>(s),
                                               std::char_traits<wchar_t>::length(s)));
    }
  }
  // char16_t* → UTF-8
  else if constexpr (std::same_as<T, const char16_t *> || std::same_as<T, char16_t *>) {
    return s ? utf16_to_utf8(std::u16string_view(s)) : "";
  }
  // char32_t* → UTF-8
  else if constexpr (std::same_as<T, const char32_t *> || std::same_as<T, char32_t *>) {
    return s ? utf32_to_utf8(std::u32string_view(s)) : "";
  }
  // UTF-8 native
  else if constexpr (std::same_as<T, std::string> || std::same_as<T, std::string_view> ||
                     std::same_as<T, std::pmr::string>) {
    return std::string(std::forward<S>(s));
  }
  // wchar_t → UTF-8
  else if constexpr (std::same_as<T, std::wstring> || std::same_as<T, std::wstring_view> ||
                     std::same_as<T, std::pmr::wstring>) {
    // Assume wchar_t is UTF-32 on Linux, UTF-16 on Windows
    if constexpr (sizeof(wchar_t) == 2)
      return utf16_to_utf8(
          std::u16string_view(reinterpret_cast<const char16_t *>(s.data()), s.size()));
    else
      return utf32_to_utf8(
          std::u32string_view(reinterpret_cast<const char32_t *>(s.data()), s.size()));
  }
  // UTF-16 → UTF-8
  else if constexpr (std::same_as<T, std::u16string> || std::same_as<T, std::u16string_view> ||
                     std::same_as<T, std::pmr::u16string>) {
    return utf16_to_utf8(std::u16string_view(s.data(), s.size()));
  }
  // UTF-32 → UTF-8
  else if constexpr (std::same_as<T, std::u32string> || std::same_as<T, std::u32string_view> ||
                     std::same_as<T, std::pmr::u32string>) {
    return utf32_to_utf8(std::u32string_view(s.data(), s.size()));
  }
  // UTF-8 pmr
  else if constexpr (std::same_as<T, std::u8string> || std::same_as<T, std::u8string_view> ||
                     std::same_as<T, std::pmr::u8string>) {
    return std::string(reinterpret_cast<const char *>(s.data()), s.size());
  } else {
    static_assert([] { return false; }(), "Unsupported string type");
  }
}

template <typename T>
  requires(std::is_arithmetic_v<T>)
auto stringify_number(T value, std::variant<bool, std::string_view> numeric_separator = false)
    -> std::string {
  if constexpr (std::is_integral_v<T>) {
    if (std::holds_alternative<bool>(numeric_separator) && !std::get<bool>(numeric_separator))
      return std::to_string(value);
    const std::string_view sep = std::holds_alternative<std::string_view>(numeric_separator)
                                     ? std::get<std::string_view>(numeric_separator)
                                     : ",";
    std::string str = std::to_string(value);
    std::reverse(str.begin(), str.end());
    for (std::size_t i = 3; i < str.size(); i += 3 + sep.size())
      str.insert(i, sep);
    std::reverse(str.begin(), str.end());
    return str;
  } else {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init)
    std::array<char, 64> buffer;
    const auto [ptr, ec] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    if (ec == std::errc{})
      return std::string(buffer.data(), ptr);
    // Fallback to std::to_string if to_chars fails
    return std::to_string(value);
  }
}

[[nodiscard]] inline auto stringify_string(const std::string &input, char quote = '"')
    -> std::string {
  std::ostringstream oss;
  oss << quote;
  for (char c : input) {
    switch (c) {
    case '\\':
      oss << "\\\\";
      break;
    case '\n':
      oss << "\\n";
      break;
    case '\r':
      oss << "\\r";
      break;
    case '\t':
      oss << "\\t";
      break;
    case '\b':
      oss << "\\b";
      break;
    case '\f':
      oss << "\\f";
      break;
    case '\v':
      oss << "\\v";
      break;
    default:
      if (c == quote)
        oss << '\\' << quote;
      else if (static_cast<unsigned char>(c) < 0x20 || static_cast<unsigned char>(c) > 0x7E)
        oss << "\\u" << std::hex << std::setw(4) << std::setfill('0') << (int)(unsigned char)c;
      else
        oss << c;
    }
  }
  oss << quote;
  return oss.str();
}

[[nodiscard]] auto stringify_node(const node::node &node, const inspect_options &options,
                                  size_t level, bool force_wrap, bool suppress_reference_pointer,
                                  size_t rest_line_length,
                                  std::unordered_map<node::ref, std::size_t, node::ref_hash> &refs)
    -> std::string;

class maximum_depth_reached : public std::exception {
public:
  [[nodiscard]] auto what() const noexcept -> const char * override {
    return "Maximum depth reached";
  }
};

template <typename T>
// NOLINTNEXTLINE(misc-no-recursion)
[[nodiscard]] auto build_tree(T &&value, const inspect_method_options &options,
                              std::unordered_map<node::ref, std::size_t, node::ref_hash> &refs)
    -> std::unique_ptr<node::node> {
  using namespace node;

  using U = std::remove_cvref_t<T>;
  const detail::c &c = options.c;

  // NOLINTNEXTLINE(misc-no-recursion)
  auto expand = [&value, &options, &refs]<typename V, typename... Os>(
                    V &&v, Os &&...opts) -> std::unique_ptr<mp::node::node> {
    inspect_method_options new_options = options;
    new_options.omitted_fields.clear();
    new_options.level++;
    new_options.ancestors.emplace_back(type_name<U>(),
                                       reinterpret_cast<std::uintptr_t>(std::addressof(value)));
    detail::patch_options(new_options, std::forward<Os>(opts)...);

    if (new_options.level > 0 && new_options.level - 1 > new_options.depth)
      throw detail::maximum_depth_reached();

    if (new_options.colors != options.colors || new_options.styles != options.styles)
      new_options.c = detail::build_c(new_options.colors, new_options.styles);

    return build_tree(std::forward<V>(v), new_options, refs);
  };

  auto inplace_expand = [&value, &options,
                         &refs]<typename V>(V &&v) -> std::unique_ptr<mp::node::node> {
    return build_tree(std::forward<V>(v), options, refs);
  };

  if constexpr (std::is_enum_v<U>) {
    return text(c.enumeration(std::string(enum_type_name_with_name(value))));
  } else if constexpr (std::same_as<U, char> || std::same_as<U, signed char> ||
                       std::same_as<U, unsigned char> || std::same_as<U, wchar_t> ||
                       std::same_as<U, char8_t> || std::same_as<U, char16_t> ||
                       std::same_as<U, char32_t>) {
    return text(c.character(stringify_string(std::string(1, value), '\'')));
  } else if constexpr (detail::string_type<U>) {
    if constexpr (std::same_as<std::decay_t<T>, char *> ||
                  std::same_as<std::decay_t<T>, const char *> ||
                  std::same_as<std::decay_t<T>, signed char *> ||
                  std::same_as<std::decay_t<T>, unsigned char *> ||
                  std::same_as<std::decay_t<T>, wchar_t *> ||
                  std::same_as<std::decay_t<T>, char8_t *> ||
                  std::same_as<std::decay_t<T>, char16_t *> ||
                  std::same_as<std::decay_t<T>, char32_t *>)
      if (!value)
        return text(c.null("nullptr"));

    const std::string str = detail::extract_string_content(value);

    const bool is_truncated = str.size() > options.max_string_length;
    const std::string truncated_str = is_truncated ? str.substr(0, options.max_string_length) : str;
    const std::string ellipsis =
        is_truncated
            ? ("... " + std::to_string(str.size() - options.max_string_length) + " more character" +
               ((str.size() - options.max_string_length == 1) ? "" : "s"))
            : "";

    auto inline_version = [&c](std::string_view value, std::string_view ellipsis = "") {
      return text(c.string(stringify_string(std::string(value))) + std::string(ellipsis));
    };

    if (truncated_str.find('\n') == std::string::npos)
      return inline_version(truncated_str, ellipsis);

    std::string rest = truncated_str;
    std::vector<std::unique_ptr<mp::node::node>> parts;
    std::size_t idx;
    while (((idx = rest.find('\n')), idx != std::string::npos)) {
      parts.push_back(inline_version(rest.substr(0, idx + 1)));
      rest = rest.substr(idx + 1);
    }
    if (rest != "")
      parts.push_back(inline_version(rest));
    for (std::size_t i = 0; i < parts.size() - 1; i++)
      std::get<text_node>(*parts[i]).value += " +";
    if (ellipsis != "")
      std::get<text_node>(*parts[parts.size() - 1]).value += ellipsis;
    return inline_wrap(inline_version(truncated_str, ellipsis), between(std::move(parts)));
  } else if constexpr (std::same_as<U, std::filesystem::path>) {
    return text("path(" + c.string(stringify_string(value.lexically_normal().string())) + ")");
  } else if constexpr (std::same_as<U, bool>) {
    return text(c.boolean(value ? "true" : "false"));
  } else if constexpr (std::is_arithmetic_v<U>) {
    return text(c.number(stringify_number(value, options.numeric_separator)));
  } else if constexpr (detail::is_specialization_v<U, std::complex>) {
    return text(
        c.number(stringify_number(value.real()) + "+" + stringify_number(value.imag()) + "i"));
  } else if constexpr (std::same_as<U, std::any>) {
    if (!value.has_value())
      return text(c.null("{}"));

    // Check some common types that std::any can hold
    // Listing more types is technically possible but will bloat the code size
    using possible_types =
        std::tuple<char, signed char, unsigned char, wchar_t, char8_t, char16_t, char32_t,
                   std::string, std::wstring, std::u8string, std::u16string, std::u32string,
                   std::string_view, std::wstring_view, std::u8string_view, std::u16string_view,
                   std::u32string_view, std::pmr::string, std::pmr::wstring, std::pmr::u8string,
                   std::pmr::u16string, std::pmr::u32string, std::filesystem::path, bool, short,
                   unsigned short, int, unsigned int, long, unsigned long, long long,
                   unsigned long long, std::complex<float>, std::complex<double>,
                   std::complex<long double>, std::any>;

    const auto &type = value.type();

    auto check_type = [&value, &type, &inplace_expand](auto id) -> std::unique_ptr<mp::node::node> {
      using V = typename decltype(id)::type;
      if (type == typeid(V))
        return inplace_expand(std::any_cast<V>(value));
      if (type == typeid(const V *const))
        return inplace_expand(std::any_cast<const V *const>(value));
      if (type == typeid(const V *))
        return inplace_expand(std::any_cast<const V *>(value));
      if (type == typeid(V *))
        return inplace_expand(std::any_cast<V *>(value));
      if (type == typeid(const V &))
        return inplace_expand(std::any_cast<const V &>(value));
      if (type == typeid(std::shared_ptr<V>))
        return inplace_expand(std::any_cast<std::shared_ptr<V>>(value));
      if (type == typeid(std::weak_ptr<V>))
        return inplace_expand(std::any_cast<std::weak_ptr<V>>(value));
      if (type == typeid(std::optional<V>))
        return inplace_expand(std::any_cast<std::optional<V>>(value));
      return nullptr;
    };

    auto res = std::unique_ptr<mp::node::node>{};
    [&check_type, &res]<std::size_t... I>(std::index_sequence<I...>) {
      (...,
       (res = res ? std::move(res)
                  : check_type(std::type_identity<std::tuple_element_t<I, possible_types>>{})));
    }(std::make_index_sequence<std::tuple_size_v<possible_types>>{});
    return res ? std::move(res) : text(c.gray("(any)"));
  } else if constexpr (detail::is_specialization_v<U, std::optional>) {
    if (!value.has_value())
      return text(c.null("nullopt"));
    return sequence(text("optional("), inplace_expand(*value), text(")"));
  }
#ifdef __cpp_lib_expected
  else if constexpr (detail::is_specialization_v<U, std::expected>) {
    if (value.has_value())
      return sequence(text("expected("), inplace_expand(value.value()), text(")"));
    return sequence(text("unexpected("), inplace_expand(value.error()), text(")"));
  }
#endif
  else if constexpr (requires { function_info<U>::arity; }) {
    using info = function_info<U>;

    const std::string prefix = []() {
      if constexpr (info::kind == function_kind::regular_function)
        return "function";
      else if constexpr (info::kind == function_kind::member_function)
        return "member function";
      else if constexpr (detail::is_specialization_v<U, std::function>)
        return "std::function";
      else
        return "functor";
    }();

    const std::string_view parameters_type_name = type_name<typename info::parameters_type>();
    const std::string parameters_sig =
        "(" + std::string(parameters_type_name.substr(11, parameters_type_name.size() - 12)) + ")";
    std::string sig;
    if constexpr (std::is_void_v<typename info::return_type>)
      sig = "void " + parameters_sig;
    else
      sig = parameters_sig + " -> " + std::string(type_name<typename info::return_type>());

    return text(c.special("[" + prefix + ": " + sig + "]"));
  } else if constexpr (std::is_pointer_v<U>) {
    return value ? sequence(text(c.special("&")), inplace_expand(*value)) : text(c.null("nullptr"));
  } else if constexpr (detail::is_specialization_v<U, std::unique_ptr>) {
    return value ? sequence(text(c.special("~")), inplace_expand(*value)) : text(c.null("nullptr"));
  } else if constexpr (detail::is_specialization_v<U, std::shared_ptr>) {
    return value ? sequence(
                       text(c.special("&[") + std::to_string(value.use_count()) + c.special("]")),
                       inplace_expand(*value))
                 : text(c.null("nullptr"));
  } else if constexpr (detail::is_specialization_v<U, std::weak_ptr>) {
    if (auto locked = value.lock()) {
      return sequence(text(c.special("&w[") + std::to_string(value.use_count()) + c.special("]")),
                      inplace_expand(*locked));
    }
    return text(c.special("&w[expired]"));
  } else if constexpr (detail::is_specialization_v<U, std::variant>) {
    return std::visit(inplace_expand, value);
  } else if constexpr (
      requires { value.inspect(options, expand); } || requires { value.inspect(options); } ||
      requires { value.inspect(); }) {
    auto as_node = [&c, &inplace_expand](auto &&res) -> std::unique_ptr<mp::node::node> {
      using U = std::decay_t<decltype(res)>;
      if constexpr (std::same_as<U, char *> || std::same_as<U, const char *>)
        return text(res ? detail::extract_string_content(res) : c.null("nullptr"));
      if constexpr (string_type<U>)
        return text(detail::extract_string_content(res));
      else if constexpr (std::same_as<U, std::unique_ptr<mp::node::node>>)
        return std::forward<decltype(res)>(res);
      else
        return inplace_expand(res);
    };

    if constexpr (requires { value.inspect(options, expand); })
      return as_node(value.inspect(options, expand));
    else if constexpr (requires { value.inspect(options); })
      return as_node(value.inspect(options));
    else
      return as_node(value.inspect());
  } else {
    constexpr auto type_name = detail::type_name<U>();
    const ref ref{std::string(type_name), reinterpret_cast<std::uintptr_t>(std::addressof(value))};

    if (options.ancestors.size() > 0 &&
        std::find(options.ancestors.begin(), options.ancestors.end(), ref) !=
            options.ancestors.end()) {
      if (const auto it = refs.find(ref); it == refs.end())
        refs[ref] = refs.size() + 1;
      return circular(value);
    }

    try {
      std::string open = "{";
      std::string close = "}";
      std::optional<std::unique_ptr<mp::node::node>> prefix;

      std::vector<std::unique_ptr<mp::node::node>> entries;
      // C-array/array/vector/list/deque
      if constexpr (std::is_array_v<U> || detail::is_std_array_v<U> ||
                    detail::is_specialization_v<U, std::vector> ||
                    detail::is_specialization_v<U, std::list> ||
                    detail::is_specialization_v<U, std::forward_list> ||
                    detail::is_specialization_v<U, std::deque>) {
        std::size_t size;
        if constexpr (std::is_array_v<U>)
          size = std::extent_v<U>;
        else if constexpr (detail::is_specialization_v<U, std::forward_list>)
          size = std::distance(value.begin(), value.end());
        else
          size = value.size();

        entries.reserve(std::min(size, options.max_sequence_length));

        std::size_t i = 0;
        for (const auto &v : value) {
          if (i >= options.max_sequence_length) {
            entries.push_back(text(c.gray("... " + std::to_string(size - i) + " more item" +
                                          (size - i > 1 ? "s" : ""))));
            break;
          }
          entries.push_back(expand(v));
          i++;
        }

        open = "[";
        close = "]";

        if constexpr (std::is_array_v<U> || detail::is_std_array_v<U>)
          prefix = text("<" + std::to_string(size) + ">");
        else if constexpr (detail::is_specialization_v<U, std::list>)
          prefix = text("list(" + std::to_string(size) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::forward_list>)
          prefix = text("forward_list(" + std::to_string(size) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::deque>)
          prefix = text("deque(" + std::to_string(size) + ") ");
      }
      // stack
      else if constexpr (detail::is_specialization_v<U, std::stack>) {
        if (value.empty())
          return text("stack(0) []");

        open = "[";
        close = "]";
        prefix = text("stack(" + std::to_string(value.size()) + ") ");

        if (value.size() == 1) {
          entries.emplace_back(sequence(text("top="), expand(value.top())));
        } else {
          entries.reserve(2);
          entries.emplace_back(sequence(text("top="), expand(value.top())));
          entries.emplace_back(text("..."));
        }
      }
      // queue
      else if constexpr (detail::is_specialization_v<U, std::queue>) {
        if (value.empty())
          return text("queue(0) []");

        open = "[";
        close = "]";
        prefix = text("queue(" + std::to_string(value.size()) + ") ");

        if (value.size() == 1) {
          entries.emplace_back(expand(value.front()));
        } else if (value.size() == 2) {
          entries.reserve(2);
          entries.emplace_back(expand(value.front()));
          entries.emplace_back(expand(value.back()));
        } else {
          entries.reserve(3);
          entries.emplace_back(expand(value.front()));
          entries.emplace_back(text("..."));
          entries.emplace_back(expand(value.back()));
        }
      }
      // priority_queue
      else if constexpr (detail::is_specialization_v<U, std::priority_queue>) {
        if (value.empty())
          return text("priority_queue(0) []");

        open = "[";
        close = "]";
        prefix = text("priority_queue(" + std::to_string(value.size()) + ") ");

        if (value.size() == 1) {
          entries.emplace_back(sequence(text("top="), expand(value.top())));
        } else {
          entries.reserve(2);
          entries.emplace_back(sequence(text("top="), expand(value.top())));
          entries.emplace_back(text("..."));
        }
      }
      // pair
      else if constexpr (detail::is_pair_v<U>) {
        return sequence(expand(value.first), text(" => "), expand(value.second));
      }
      // tuple
      else if constexpr (detail::is_tuple_v<U>) {
        entries.reserve(std::tuple_size_v<U>);
        std::apply(
            [&entries, &expand](const auto &...elements) {
              entries.reserve(sizeof...(elements));
              (entries.push_back(expand(elements)), ...);
            },
            value);

        open = "(";
        close = ")";
      }
      // unordered_map/map, unordered_multimap/multimap
      else if constexpr (detail::is_specialization_v<U, std::unordered_map> ||
                         detail::is_specialization_v<U, std::map> ||
                         detail::is_specialization_v<U, std::unordered_multimap> ||
                         detail::is_specialization_v<U, std::multimap>) {
        entries.reserve(value.size());
        for (const auto &[key, value] : value)
          entries.push_back(sequence(expand(key), text(" => "), expand(value)));

        if constexpr (detail::is_specialization_v<U, std::unordered_map>)
          prefix = text("unordered_map(" + std::to_string(value.size()) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::map>)
          prefix = text("map(" + std::to_string(value.size()) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::unordered_multimap>)
          prefix = text("unordered_multimap(" + std::to_string(value.size()) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::multimap>)
          prefix = text("multimap(" + std::to_string(value.size()) + ") ");

        if ((detail::is_specialization_v<U, std::unordered_map> ||
             detail::is_specialization_v<U, std::unordered_multimap>) &&
            options.sorted)
          std::sort(entries.begin(), entries.end(),
                    [](const std::unique_ptr<mp::node::node> &a,
                       const std::unique_ptr<mp::node::node> &b) {
                      const auto &key_a = *std::get<sequence_node>(*a).values[0];
                      const auto &key_b = *std::get<sequence_node>(*b).values[0];

                      inspect_options key_options;
                      key_options.indent = 0;
                      key_options.colors = false;
                      std::unordered_map<mp::node::ref, std::size_t, mp::node::ref_hash> dummy_refs;

                      return stringify_node(key_a, key_options, 0, false, false, 0, dummy_refs) <
                             stringify_node(key_b, key_options, 0, false, false, 0, dummy_refs);
                    });
      }
      // unordered_set/set, unordered_multiset/multiset
      else if constexpr (detail::is_specialization_v<U, std::unordered_set> ||
                         detail::is_specialization_v<U, std::set> ||
                         detail::is_specialization_v<U, std::unordered_multiset> ||
                         detail::is_specialization_v<U, std::multiset>) {
        entries.reserve(value.size());
        for (const auto &v : value)
          entries.push_back(expand(v));

        if constexpr (detail::is_specialization_v<U, std::unordered_set>)
          prefix = text("unordered_set(" + std::to_string(value.size()) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::set>)
          prefix = text("set(" + std::to_string(value.size()) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::unordered_multiset>)
          prefix = text("unordered_multiset(" + std::to_string(value.size()) + ") ");
        else if constexpr (detail::is_specialization_v<U, std::multiset>)
          prefix = text("multiset(" + std::to_string(value.size()) + ") ");

        if ((detail::is_specialization_v<U, std::unordered_set> ||
             detail::is_specialization_v<U, std::unordered_multiset>) &&
            options.sorted)
          std::sort(entries.begin(), entries.end(),
                    [](const std::unique_ptr<mp::node::node> &a,
                       const std::unique_ptr<mp::node::node> &b) {
                      inspect_options key_options;
                      key_options.indent = 0;
                      key_options.colors = false;
                      std::unordered_map<mp::node::ref, std::size_t, mp::node::ref_hash> dummy_refs;

                      return stringify_node(*a, key_options, 0, false, false, 0, dummy_refs) <
                             stringify_node(*b, key_options, 0, false, false, 0, dummy_refs);
                    });
      }
      // Regular aggregate object
      else if constexpr (std::is_aggregate_v<U>) {
        constexpr std::size_t fields_count = count_fields<U>();
        entries.reserve(fields_count);
        const auto fields = tie_as_tuple(value, size_t_<fields_count>{});

        // NOLINTNEXTLINE(misc-no-recursion)
        auto append_entry = [&options, &expand, &entries, &fields]<std::size_t I>(
                                std::integral_constant<std::size_t, I> /*index*/) {
          const std::string name_s = std::string(field_name<U, I>());
          // NOLINTNEXTLINE(readability-container-contains)
          if (options.omitted_fields.find(name_s) != options.omitted_fields.end())
            return;
          entries.push_back(sequence(text(name_s + ": "), expand(sequence_tuple::get<I>(fields))));
        };

        // NOLINTNEXTLINE(misc-no-recursion)
        [&append_entry]<std::size_t... I>(std::index_sequence<I...>) {
          (append_entry(std::integral_constant<std::size_t, I>{}), ...);
        }(std::make_index_sequence<fields_count>{});

        prefix = text(std::string(type_name) + " ");

        if (options.sorted)
          std::sort(entries.begin(), entries.end(),
                    [](const std::unique_ptr<mp::node::node> &a,
                       const std::unique_ptr<mp::node::node> &b) {
                      const std::string a_str =
                          std::get<text_node>(*std::get<sequence_node>(*a).values[0]).value;
                      const std::string b_str =
                          std::get<text_node>(*std::get<sequence_node>(*b).values[0]).value;
                      return a_str < b_str;
                    });
      }
      // Non-aggregate object fallback
      else {
        return text(inspect_fallback(value, options));
      }

      const bool brace_spacing = open == "["   ? options.sequence_bracket_spacing
                                 : open == "{" ? options.object_curly_spacing
                                               : false;

      if (entries.empty())
        return text((detail::is_specialization_v<U, std::vector> ? "" : std::string(type_name)) +
                    open + close);

      std::vector<std::unique_ptr<mp::node::node>> entries2; // For wrap node to use
      entries2.reserve(entries.size());
      for (const auto &entry : entries)
        entries2.push_back(clone_node(*entry));

      std::vector<std::unique_ptr<mp::node::node>> inline_entries;
      for (std::size_t i = 0; i < entries.size(); i++) {
        std::unique_ptr<mp::node::node> entry = std::move(entries[i]);
        if (i != entries.size() - 1)
          inline_entries.push_back(sequence(std::move(entry), text(", ")));
        else if (options.trailing_comma == option::trailing_comma_type::always)
          inline_entries.push_back(sequence(std::move(entry), text(",")));
        else
          inline_entries.push_back(std::move(entry));
      }
      std::unique_ptr<mp::node::node> inline_variant =
          between(std::move(inline_entries), text(open + (brace_spacing ? " " : "")),
                  text((brace_spacing ? " " : "") + close));

      std::vector<std::unique_ptr<mp::node::node>> wrap_entries;
      for (std::size_t i = 0; i < entries2.size(); i++) {
        std::unique_ptr<mp::node::node> entry = std::move(entries2[i]);
        if (options.trailing_comma != option::trailing_comma_type::none || i != entries2.size() - 1)
          wrap_entries.push_back(sequence(std::move(entry), text(",")));
        else
          wrap_entries.push_back(std::move(entry));
      }
      std::unique_ptr<mp::node::node> wrap_variant =
          between(std::move(wrap_entries), text(open), text(close));

      std::unique_ptr<mp::node::node> body =
          inline_wrap(std::move(inline_variant), std::move(wrap_variant));

      std::unique_ptr<mp::node::node> res =
          prefix.has_value() ? sequence(std::move(*prefix), std::move(body)) : std::move(body);

      std::visit([&ref](auto &node) { node.ref = ref; }, *res);

      return res;
    } catch (const detail::maximum_depth_reached &) {
      return text(inspect_fallback(value, options));
    }
  }
}

[[nodiscard]] auto inline stringify_node(
    const node::node &node, const inspect_options &options, size_t level, bool force_wrap,
    bool suppress_reference_pointer, size_t rest_line_length,
    std::unordered_map<node::ref, std::size_t, node::ref_hash> &refs) -> std::string {
  using namespace node;

  return std::visit(
      [&options, &refs, level, force_wrap, suppress_reference_pointer,
       rest_line_length](auto &&node) {
        using T = std::decay_t<decltype(node)>;

        if constexpr (std::same_as<T, circular_node>) {
          const std::string str = options.reference_pointer
                                      ? "[Circular *" + std::to_string(refs[node.ref]) + "]"
                                      : "[Circular]";
          return options.colors ? detail::colorize(str, options.styles.special) : str;
        } else {
          std::string res;

          auto get_rest_line_length = [&res, &options, rest_line_length]() {
            if (res.find('\n') == std::string::npos)
              return rest_line_length - res.size();
            return options.break_length - (res.size() - (res.rfind('\n') + 1));
          };

          // text
          if constexpr (std::same_as<T, text_node>) {
            res = std::string(node.value);
          }

          // variant
          else if constexpr (std::same_as<T, inline_wrap_node>) {
            if (!force_wrap) {
              inspect_options new_options = options;
              new_options.indent = 0;
              res = stringify_node(*node.inline_node, new_options, level, force_wrap, false,
                                   rest_line_length, refs);
            }

            if (force_wrap || (options.indent && clean_ansi(res).size() > rest_line_length))
              res = stringify_node(*node.wrap_node, options, level, true, false,
                                   get_rest_line_length(), refs);
          }

          // sequence
          else if constexpr (std::same_as<T, sequence_node>) {
            const bool is_pointer = [&node]() {
              if (node.values.size() != 2 || !std::holds_alternative<text_node>(*node.values[0]))
                return false;
              const std::string prefix = clean_ansi(std::get<text_node>(*node.values[0]).value);
              return prefix == "~" || prefix.starts_with("&");
            }();

            if (!force_wrap)
              for (const auto &value : node.values) {
                inspect_options new_options = options;
                new_options.indent = 0;
                res += stringify_node(*value, new_options, level, force_wrap, is_pointer,
                                      rest_line_length, refs);
              }

            if (force_wrap || (options.indent && clean_ansi(res).size() > rest_line_length)) {
              res = "";
              for (const auto &value : node.values)
                res += stringify_node(*value, options, level, true, is_pointer,
                                      get_rest_line_length(), refs);
            }

            if (options.reference_pointer && is_pointer &&
                !std::holds_alternative<circular_node>(*node.values[1]))
              if (const auto it =
                      refs.find(std::visit([](auto &&node) { return node.ref; }, *node.values[1]));
                  it != refs.end()) {
                const std::string str = "<ref *" + std::to_string(it->second) + ">";
                res = (options.colors ? detail::colorize(str, options.styles.special) : str) + " " +
                      res;
              }
          }

          // between
          else if constexpr (std::same_as<T, between_node>) {
            if (!force_wrap) {
              inspect_options new_options = options;
              new_options.indent = 0;
              res = node.open ? stringify_node(*node.open, new_options, level, force_wrap, false,
                                               rest_line_length, refs)
                              : "";
              for (const auto &value : node.values)
                res += stringify_node(*value, new_options, level, force_wrap, false,
                                      rest_line_length, refs);
              res += node.close ? stringify_node(*node.close, new_options, level, force_wrap, false,
                                                 rest_line_length, refs)
                                : "";
            }

            if (force_wrap || (options.indent && clean_ansi(res).size() > rest_line_length)) {
              res = node.open ? stringify_node(*node.open, options, level, false, false,
                                               rest_line_length, refs)
                              : "";
              for (std::size_t i = 0; i < node.values.size(); ++i) {
                if (i != 0 || !res.empty())
                  res += "\n" + std::string((level + 1) * options.indent, ' ');
                res += stringify_node(*node.values[i], options, level + 1, false, false,
                                      get_rest_line_length(), refs);
              }
              const std::string after = node.close
                                            ? stringify_node(*node.close, options, level, false,
                                                             false, get_rest_line_length(), refs)
                                            : "";
              if (!after.empty())
                res += (node.values.size() ? "\n" + std::string(level * options.indent, ' ') : "") +
                       after;
            }
          }

          if (!suppress_reference_pointer && options.reference_pointer && node.ref.second)
            if (const auto it = refs.find(node.ref); it != refs.end()) {
              const std::string str = "<ref *" + std::to_string(it->second) + ">";
              res = (options.colors ? detail::colorize(str, options.styles.special) : str) + " " +
                    res;
            }

          return res;
        }
      },
      node);
}

} // namespace detail

template <bool UseGlobalDefaultOptions = false, typename T, typename... Os>
  requires(std::is_base_of_v<detail::setting<typename Os::type, Os::tag>, Os> && ...)
[[nodiscard]] auto inspect(T &&value, Os &&...options) -> std::string {
  inspect_method_options build_tree_options;
  if constexpr (UseGlobalDefaultOptions)
    detail::patch_options_with_global_default_options(build_tree_options);
  detail::patch_options(build_tree_options, std::forward<Os>(options)...);
  build_tree_options.c = detail::build_c(build_tree_options.colors, build_tree_options.styles);
  std::unordered_map<node::ref, std::size_t, node::ref_hash> refs;

  const auto node = detail::build_tree(std::forward<T>(value), build_tree_options, refs);

  inspect_options stringify_node_options;
  if constexpr (UseGlobalDefaultOptions)
    detail::patch_options_with_global_default_options(stringify_node_options);
  detail::patch_options(stringify_node_options, std::forward<Os>(options)...);

  return detail::stringify_node(*node, stringify_node_options, 0, false, false,
                                stringify_node_options.break_length, refs);
}

namespace detail {

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline std::mutex print_mutex;

template <typename Arg> [[nodiscard]] inline auto stringify_print_arg(Arg &&arg) -> std::string {
  if constexpr (std::same_as<Arg, const char *> || std::same_as<Arg, char *>)
    return arg ? detail::extract_string_content(std::forward<Arg>(arg))
               : inspect<true>(std::forward<Arg>(arg));
  else if constexpr (detail::string_type<Arg>)
    return detail::extract_string_content(std::forward<Arg>(arg));
  else
    return inspect<true>(std::forward<Arg>(arg));
}

} // namespace detail

template <typename... Args> void print(Args &&...args) {
  std::scoped_lock lock(detail::print_mutex);
  bool first = true;

  auto print_arg = [&first](auto &&arg) {
    if (!first)
      // NOLINTNEXTLINE(cert-err33-c)
      std::fputc(' ', stdout);
    const std::string str = detail::stringify_print_arg(std::forward<decltype(arg)>(arg));
    // NOLINTNEXTLINE(cert-err33-c)
    std::fwrite(str.data(), 1, str.size(), stdout);
    first = false;
  };

  (print_arg(std::forward<Args>(args)), ...);
}

template <typename... Args> void println(Args &&...args) {
  std::scoped_lock lock(detail::print_mutex);
  bool first = true;

  auto print_arg = [&first](auto &&arg) {
    if (!first)
      // NOLINTNEXTLINE(cert-err33-c)
      std::fputc(' ', stdout);
    const std::string str = detail::stringify_print_arg(std::forward<decltype(arg)>(arg));
    // NOLINTNEXTLINE(cert-err33-c)
    std::fwrite(str.data(), 1, str.size(), stdout);
    first = false;
  };

  (print_arg(std::forward<Args>(args)), ...);

  // NOLINTNEXTLINE(cert-err33-c)
  std::fputc('\n', stdout);
}

} // namespace mp

#endif // MEGAPRINT_HPP
