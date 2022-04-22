"""
反转整数
123>>321
-123>>-321
120>>21
异常点：负数符号，首位0，int上限溢出(python没有上限)
解法：
1.首尾互换
2.数学整除取余
3.str[::-1]
"""


# 使用list来首尾互换
def solution_a(num: int):
    # 负数flag
    flag = False
    # 转str
    num = str(num)
    if num.startswith('-'):
        flag = True
        num = num[1:]
    # 转成list
    L = [x for x in num]
    S = 0
    E = len(L) - 1
    while S < E:
        # 首尾交换
        L[S], L[E] = L[E], L[S]
        # tmp = L[S]
        # L[S] = L[E]
        # L[E] = tmp
        # 移动指针
        S += 1
        E -= 1
    # 把list转成str，添加-
    res = (''.join(L))
    res = '-' + res if flag else res
    try:
        res = int(res)
        return res
    except Exception as e:
        print(e)
        return None


print(solution_a(123456789))
print(solution_a(-123456789))
print(solution_a(123456789000))


# 使用数学方法就不需要使用list
def solution_b(num: int):
    # 先判断负数,* -1 转成正数
    flag = False
    if num < 0:
        flag = True
        num = num * -1
    else:
        pass

    reversed_num = 0
    while num > 0:
        # % 取余数
        remainder = num % 10
        reversed_num = reversed_num * 10 + remainder
        # // 取整数
        num = num // 10
    reversed_num = reversed_num * -1 if flag else reversed_num
    return reversed_num


print(solution_b(987654321))
print(solution_b(-987654321))
print(solution_b(9876543210000000))
