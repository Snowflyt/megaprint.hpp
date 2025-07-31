#include <queue>
#include <stack>

#include <megaprint/megaprint.hpp>

auto main() -> int {
  std::stack<int> stk;
  mp::println("stack:", stk);
  stk.push(1);
  mp::println("stack:", stk);
  stk.push(2);
  mp::println("stack:", stk);
  stk.push(3);
  mp::println("stack:", stk);
  stk.push(4);
  mp::println("stack:", stk);

  std::queue<int> que;
  mp::println("queue:", que);
  que.push(1);
  mp::println("queue:", que);
  que.push(2);
  mp::println("queue:", que);
  que.push(3);
  mp::println("queue:", que);
  que.push(4);
  mp::println("queue:", que);

  std::priority_queue<int> p_que;
  mp::println("priority_queue:", p_que);
  p_que.push(3);
  mp::println("priority_queue:", p_que);
  p_que.push(4);
  mp::println("priority_queue:", p_que);
  p_que.push(1);
  mp::println("priority_queue:", p_que);
  p_que.push(2);
  mp::println("priority_queue:", p_que);

  return 0;
}
