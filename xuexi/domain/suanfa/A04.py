"""
队列queue，先进先出的数据类型
head,tail,size,add(),poll()
求t-3000内的请求数量
input=[100,200,300,500,1000,2000,3000,3001,3002]
output=[1,2,3,4,5,6,7,7,7]
"""
import queue

q = queue.Queue()


class Node:
    def __init__(self, val=None):
        self.val = val
        self.next = None


class QueueSimple:
    def __init__(self):
        self.head = None
        self.tail = None
        self.size = 0

    # 始终再尾部追加数据
    def add(self, item) -> Node:
        new = Node(item)
        # 判断是否首次添加
        if self.head is None:
            self.head = new
            self.tail = new
        else:
            last = self.tail
            self.tail = new
            last.next = self.tail
        self.size += 1
        return self.tail

    # 始终从头节点移除数据
    def poll(self) -> Node:
        if self.head is None:
            return Node()
        head = self.head
        second = self.head.next
        if second:
            self.head = second
        else:
            self.head = None
            self.tail = None
        self.size -= 1
        return head


# a = QueueSimple()
# print(a.add('1').val)
# print(a.add('2').val)
# print(a.add('3').val)
# print(a.add('4').val)
# print(a.poll().val)
# print(a.poll().val)
# print(a.poll().val)
# print(a.poll().val)
# print(a.poll().val)

# 使用队列，计算区间，维持一个只在区间内的队列
class PCount:
    def __init__(self):
        self.q = QueueSimple()

    def solution(self, t):
        self.q.add(t)
        while int(self.q.head.val) < t - 3000:
            self.q.poll()
        return self.q


L = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000]
a = PCount()
for i in L:
    res = a.solution(i)
    print(res.size, res.head.val)
