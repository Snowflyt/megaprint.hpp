#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

#include <megaprint/megaprint.hpp>

template <typename T> class Box {
public:
  explicit Box(T value) : value_(std::move(value)) {}

  // Custom inspect function
  [[nodiscard]] auto inspect(const auto & /*options*/, const auto &expand) const {
    using namespace mp::node;
    // The return type of user-defined inspect function can be one of the following:
    // * 'std::unique_ptr<mp::node::node>' created by helper functions in 'mp::node'
    //   namespace (sequence, text, etc.), which creates a tree-like structure that
    //   automatically handles indentation and max recursion depth.
    // * 'std::string' or any other string-like type, which will be printed as plain text.
    //   Note that you should handle indentation and formatting yourself in this case.
    // * Any other value that will be inspected instead of the object itself.
    return sequence(text("Box("), expand(value_), text(")"));
  }

private:
  T value_;
};

class Point {
public:
  Point(double x, double y) : x_(x), y_(y) {}

  [[nodiscard]] auto inspect() const {
    return "Point(" + mp::inspect(x_) + ", " + mp::inspect(y_) + ")";
  }

private:
  double x_;
  double y_;
};

template <typename T, typename U> class Pair1 {
public:
  Pair1(T left, U right) : left_(std::move(left)), right_(std::move(right)) {}

  [[nodiscard]] auto inspect(const auto & /*options*/, const auto &expand) const {
    using namespace mp::node;
    return sequence(text("("), expand(left_), text(", "), expand(right_), text(")"));
  }

private:
  T left_;
  U right_;
};

template <typename T, typename U> class Pair2 {
public:
  Pair2(T left, U right) : left_(std::move(left)), right_(std::move(right)) {}

  [[nodiscard]] auto inspect(const auto &options, const auto &expand) const {
    // Alternatively, you can use `options.n` to access node manipulation functions
    // without needing to include the `megaprint.hpp` header in your code
    const auto &n = options.n;
    using node = std::invoke_result_t<decltype(n.text), std::string_view>;
    std::vector<node> children;
    children.emplace_back(n.sequence(expand(left_), n.text(", ")));
    children.emplace_back(expand(right_));
    return n.between(std::move(children), n.text("("), n.text(")"));
  }

private:
  T left_;
  U right_;
};

template <typename T, typename U> class Pair3 {
public:
  Pair3(T left, U right) : left_(std::move(left)), right_(std::move(right)) {}

  [[nodiscard]] auto inspect(const auto & /*options*/, const auto &expand) const {
    using namespace mp::node;
    std::vector<std::unique_ptr<node>> children;
    children.emplace_back(sequence(expand(left_), text(","))); // No space after the comma
    children.emplace_back(expand(right_));
    return inline_wrap(
        // The inline format
        sequence(text("("), expand(left_), text(", "), // A comma with a space after
                 expand(right_), text(")")),
        // The multiline format
        between(std::move(children), text("("), text(")")));
  }

private:
  T left_;
  U right_;
};

template <typename T> class Wrapper {
public:
  explicit Wrapper(std::string_view tag, T value) : tag_(tag), value_(std::move(value)) {}

  [[nodiscard]] auto inspect(const auto &options, const auto &expand) const {
    using namespace mp::node;
    return sequence(text(options.c.blue(tag_)), text("("),
                    expand(value_,
                           // Reset `level` and force expansion the object with a depth of 1
                           mp::option::level{0U}, mp::option::depth{1U},
                           // Omit the `value` field when expanding the object
                           mp::option::omitted_fields{std::unordered_set<std::string>{"value"}},
                           // Avoid auto-adding current value to `ancestors` to avoid circulars
                           mp::option::ancestors{options.ancestors}),
                    text(")"));
  }

private:
  std::string tag_;
  T value_;
};

struct Foo {
  std::string foo;
};

// NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init)
struct Obj {
  Point point;
  Box<Foo> box1;
  Box<Point> box2;
};

struct FooBarBaz {
  std::string foo;
  std::string bar;
  std::string baz;
};

struct Baz {
  std::string baz;
};

struct Bar {
  Baz bar;
};

// NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init)
struct MyType {
  int value;
  Bar nested;
};

auto main() -> int {
  const Obj obj = {
      .point = Point{1, 2},
      .box1 = Box{Foo{.foo = "bar"}},
      .box2 = Box{Point{3, 4}},
  };

  mp::println(obj);

  const Pair1 p1{"left", "right"};
  const Pair2 p2{"left", "right"};

  mp::println(p1);
  // ("left", "right")
  mp::println(p2);
  // ("left", "right")

  const std::string s1 = "This is a very long string that will break the line";
  const std::string s2 = "This is another very long string that will break the line";

  const Pair1 p3{s1, s2};
  const Pair2 p4{s1, s2};

  mp::println(p3);
  // ("This is a very long string that will break the line", "This is another very long string that
  // will break the line")
  mp::println(p4);
  // (
  //   "This is a very long string that will break the line",
  //   "This is another very long string that will break the line"
  // )

  const FooBarBaz obj1 = {.foo = "bar", .bar = "baz", .baz = "quxx"};
  const FooBarBaz obj2 = {.foo = "baz", .bar = "bar", .baz = "foo"};

  const Pair1 p5{obj1, obj2};
  const Pair2 p6{obj1, obj2};

  mp::println(p5);
  // (FooBarBaz {
  //   foo: "bar",
  //   bar: "baz",
  //   baz: "quux"
  // }, FooBarBaz {
  //   qux: "baz",
  //   baz: "bar",
  //   bar: "foo"
  // })
  mp::println(p6);
  // (
  //   FooBarBaz { foo: "bar", bar: "baz", baz: "quux" },
  //   FooBarBaz { qux: "baz", baz: "bar", bar: "foo" }
  // )

  const MyType my_type = {
      .value = 42,
      .nested = Bar{.bar = Baz{.baz = "qux"}},
  };

  mp::println(Wrapper{"Foo", my_type});
  // Foo(MyType { nested: Bar { bar: [Baz@000000e8f8aff1d8] } })

  return 0;
}
