"""
2个数字链表相加
给定链表是从个位数排列的
[111]+[123]=432
异常点：进位，首位0，长度不一样
解法：
对位相加,利用链表next特性往后进位
"""


def solution_a(l1: list, l2: list):
    # 先判断下长度，前短后长
    if len(l1) <= len(l2):
        li1 = iter(l1)
        li2 = iter(l2)
    else:
        li1 = iter(l2)
        li2 = iter(l1)
    # 进位flag
    flag = False
    res = []
    while True:
        # 按顺序取出位数
        try:
            x = next(li1)
        except Exception as e:
            x = 0
        try:
            y = next(li2)
        except Exception as e:
            break
        # 相加计算
        if flag:
            z = x + y + 1
            flag = False
        else:
            z = x + y
        if z > 10:
            flag = True
            z = z % 10
        else:
            pass
        res.append(str(z))
    # 转成str倒序转回int
    a = ''.join(res)
    return int(a[::-1])


L1 = [2, 7, 6, 9, 3, 1]
L2 = [0, 1, 5, 4, 0]
print(solution_a(L1, L2))
