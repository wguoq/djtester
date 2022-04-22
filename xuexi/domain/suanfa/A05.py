"""
判断一个链表里是否有环
快慢指针，步长2和1，一起跑，如果追上了就有环，否则不是
有环是返回环起点位置
"""


# 先写个链表
class Node:
    def __init__(self, val):
        self.val = val
        self.next = None


class Solution(object):

    def detectCycle(self, head):
        fast, slow = head, head
        while True:
            # 如果fast走到头了就是没有环
            if not (fast and fast.next):
                return False
            fast, slow = fast.next.next, slow.next
            # 相遇就是有环
            if fast == slow:
                break
        # 寻找环的入口在哪，
        # 因为fast和slow相遇一定是在环里面
        # 这时候把任何一个移动回起点，然后以同样的速度跑
        # 那么2个节点只可能在入口相遇
        fast = head
        while fast != slow:
            fast, slow = fast.next, slow.next
        return fast
