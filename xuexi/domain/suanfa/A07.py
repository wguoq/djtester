"""
删除排序数组中的重复项，只能在原数组上操作
解法：双指针+原数组赋值，
[0, 0, 1, 1, 1, 2, 2, 3, 3, 4] >> [0,1,2,3,4]
 ^  ^
 a指针用来保证没有重复，b指针用来遍历
 根据赋值次数来截取长度
"""


def solution_a(target: list):
    if target is None or len(target) == 0:
        return 0
    a = 0
    # b指针从第二个位置开始走
    for b in range(1, len(target)):
        # 如果a,b值不同，那么a往前走一步，否则a不动
        if target[a] == target[b]:
            continue
        else:
            a += 1
            # 如果a,b没有走到一起说没到底，赋值
            if a != b:
                target[a] = target[b]
    return target[:a + 1]


a = [0, 0, 1, 1, 1, 2, 3, 3, 3, 4, 5]
print(solution_a(a))

"""
给你一个数组 nums 和一个值 val，你需要 原地 移除所有数值等于 val 的元素，并返回移除后数组的新长度。

不要使用额外的数组空间，你必须仅使用 O(1) 额外空间并 原地 修改输入数组。
给定 nums = [0,1,2,2,3,0,4,2], val = 2,

函数应该返回新的长度 5, 并且 nums 中的前五个元素为 0, 1, 3, 0, 4。

注意这五个元素可为任意顺序。
"""


def solution_b(nums: list, val: int):
    if nums is None or len(nums) == 0:
        return 0
    else:
        # a来保证移除，b来遍历找不等于val的
        flag = 0
        a = 0
        b = 1
        while b <= len(nums) - 1:
            if nums[a] == val:
                # 等待被替换
                if nums[b] == val:
                    b += 1
                else:
                    nums[a], nums[b] = nums[b], nums[a]
                    a += 1
                    b += 1
                    flag +=1
            else:
                a += 1
                b += 1
    return nums[:len(nums)-flag]


a = [0, 1, 2, 2, 3, 0, 4, 2]
print(solution_b(a, 2))
b =[3,2,2,3]
print(solution_b(b, 3))
