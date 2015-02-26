#ifndef shared_list_H_
#define shared_list_H_

#include <memory>
#include <cassert>

template<class T>
class shared_list {
public:
  shared_list() : _size(0) {}

  shared_list(T head, shared_list const & tail)
    : _node(std::make_shared<Node>(head, tail._node))
    , _size(1 + tail.size()) {}

  template <typename ...Ts>
  shared_list(T head, Ts... tail)
    : shared_list<T>(head, shared_list<T>(tail...)) {}

  shared_list(shared_list const & lhs, shared_list const & rhs)
    : _size(lhs.size() + rhs.size()) {
    auto lhsNode = lhs._node;
    auto * nodePtr = &_node;
    while (lhsNode) {
      *nodePtr = std::make_shared<Node>(lhsNode->_value);
      nodePtr = &((*nodePtr)->_next);
      lhsNode = lhsNode->_next;
    }
    *nodePtr = rhs._node;
  }

  inline auto isEmpty() const -> bool {
    return not _node;
  }

  using size_type = size_t;

  inline auto size() const -> size_type {
    if (_node and _size == 0) {
      return _node->size();
    }
    return _size;
  }

  inline auto head() const -> T {
    assert(not isEmpty());
    return _node->_value;
  }

  inline auto tail() const -> shared_list<T> {
    return _node ? shared_list<T>(_node->_next) : shared_list<T>();
  }

  inline auto drop(size_type n) const -> shared_list<T> {
    auto node = _node;
    for (auto i = 0; i < n and node; i++) {
      node = node->_next;
    }
    return shared_list<T>(node);
  }

  inline auto operator [](size_t n) const -> T {
    assert(n < size());
    auto node = _node.get();
    for (auto i = 0; i < n; i++) {
      node = node->_next.get();
    }
    return node->_value;
  }

private:
  struct Node {
      Node(T value) : _value(value) {}

      Node(T value, std::shared_ptr<Node> next)
        : _value(value)
        , _next(next) {}

      ~Node() {
        // Unroll recursive destructions to avoid blowing the stack
        auto node = std::move(_next);
        while (node.unique()) {
          node = std::move(node->_next);
        }
      }

      auto size() -> size_type const {
        size_type count = 1;
        auto node = _next.get();
        while (node) {
          ++count;
          node = node->_next.get();
        }
        return count;
      }

      T _value;
      std::shared_ptr<Node> _next;
  };

private:
  std::shared_ptr<Node> _node;
  size_type _size;

  shared_list(std::shared_ptr<Node> node)
      : _node(node)
      , _size(0) {}
};

#endif // shared_list_H_
