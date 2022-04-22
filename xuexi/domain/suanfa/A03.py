"""
栈，是后进先出的数据结构，用于找出成对出现的符号
删除最外层的（）
((a)(b))((c))(d)>>(a)(b)(c)d
使用列表方法实现堆栈非常容易，最后插入的最先取出（“后进先出”）。
把元素添加到堆栈的顶端，使用 append() 。从堆栈顶部取出元素，使用 pop() ，不用指定索引
解法：遍历，计数成对出现的符号
"""


# 用截取字符串的方式解
def solution_a(T: str):
    # flag用来标记是否找到成对的符号
    flag = 0
    # # S用来记录遍历到list哪里
    S = 0
    res = ''
    for i in range(len(T)):
        if T[i] == '(':
            flag += 1
        elif T[i] == ')':
            flag -= 1
        else:
            # 处理并没有被括号包含的字符串
            if flag == 0:
                res = res + T[i]
            else:
                pass
        # 找到成对的，把最外面的（）丢掉
        if flag == 0:
            E = i
            res = res + T[S + 1:E]
            S = E + 1
        else:
            continue
    return res


a = '((a)(b))((c))(d)'
print(solution_a(a))
b = '((a)(b))(((c)))(d)e'
print(solution_a(b))
c = 'c((a)(b))x(((c)))(d)e'
print(solution_a(c))


# 用栈的思想解，可以免去截取字符串时复杂的位数计算
# 把str逐个入栈，通过flag判断是否成对的()，然后出栈
def solution_b(T: str):
    # 用list做栈,因为list的pop方法默认移除末尾
    stack_ = []
    # 设计当flag=0时出栈，那么由于第一个（一定要移除所以从-1开始
    flag = 0
    res = ''
    for x in T:
        if x == '(':
            flag += 1
            stack_.append(x)
        elif x == ')':
            flag -= 1
            stack_.append(x)
        else:
            # 不是（）的都存起来,特殊处理首尾
            if flag == 0:
                res = res + x
            else:
                stack_.append(x)
        # 找到一队，把首尾（）去掉
        if flag == 0 and len(stack_) > 1:
            stack_.pop(0)
            stack_.pop()
            res = res + ''.join(stack_)
            stack_.clear()
        else:
            pass
    return res


a = 'x((a)(b)z)((c))(d)y'
print(solution_b(a))


class Stack:
    def __init__(self):
        self.list = []
        self.count = 0

    def push(self, item):
        self.list.append(item)
        self.count += 1

    def pop(self):
        if self.count - 1 >= 0:
            a = self.list.pop(self.count - 1)
            self.count -= 1
            return a
        else:
            return None

    def is_empty(self):
        return True if self.count == 0 else False


new = Stack()

# new.push(1)
# new.push(2)
# new.push(3)
# new.push(4)
#
# print(new.pop())
# print(new.is_empty())
# print(new.pop())
# print(new.is_empty())
# print(new.pop())
# print(new.is_empty())
# print(new.pop())
# print(new.is_empty())
# print(new.pop())
# print(new.is_empty())
