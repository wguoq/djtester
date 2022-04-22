"""
跳表，Redis的索引实现
index层级为总数据长度的1/64为最优

start-> 1                      80
        1          40          80
        1,   20    40    60    80
        1,10,20,30,40,50,60,70,80,90

保证start节点永远处于左上角，
那么每次从start.right开始查询
"""
import math
import random


class Node:
    def __init__(self, val, right=None, down=None):
        self.val = val
        self.right = right
        self.down = down


class SkipList:
    def __init__(self):
        # 初始化start
        self.start = Node(-1, None, None)
        # 链表总长度
        self.length = 1
        # 当前层级
        self.levels = 1

    class Locate:
        def __init__(self, node: Node, path: []):
            self.node = node
            self.path = path

    def _locate(self, tar):
        node = self.start
        path = []
        res = self.Locate(node=node, path=[])
        # 从上到下的循环
        while node is not None:
            # 从左到右的循环
            # 如果值大于当前右边的，那么说明在当前区间外，那么先往右移动一个区间
            while node.right is not None and tar > node.right.val:
                node = node.right
            # 否则就落在了当前区间，
            # 记录每一层的这个节点，为了给删除用
            # 然后往下层找
            path.append(node)
            # print(path)
            res = self.Locate(node=node, path=path)
            node = node.down
        # 返回在最下层定位的节点
        return res

    def search(self, val):
        locale = self._locate(val)
        node = locale.node
        if node.right is not None and node.right.val == val:
            return node
        else:
            return None

    def _add_new_level(self, down_node: Node, new: Node):
        print(f'_add_new_level == {new.val}')
        # 在左上角新加一层
        right = self.start.right
        new_node = Node(new.val, None, down_node)
        node1 = Node(right.val, new_node, right)
        self.start.right = node1
        self.levels += 1

    def _add_index(self, down_node: Node, left_node: Node, new: Node):
        print(f'_add_index == {new.val}')
        new_node = Node(new.val, left_node.right, down_node)
        left_node.right = new_node

    def add(self, val):
        locate = self._locate(val)
        node = locate.node
        right = node.right or None
        new = Node(val=val)
        node.right = new
        new.right = right
        self.length += 1
        # 数据总长小于8或者索引层数超过原链表数据量的对数（以2为底）就不加索引了
        if self.length < 8 or (self.levels - 1) > int(math.log(self.length, 2)):
            return node.right
        else:
            path = locate.path
            path_len = len(locate.path)
            if path_len == 1:
                # path只有一个就说明只有一层，直接新加一层
                self._add_new_level(path[0], new)
                return node.right
            else:
                x = 0
                # 遍历path添加索引，每添加一层都roll一次点
                for i in range(len(path)-1):
                    if random.randint(0, 1) == 0:
                        break
                    else:
                        self._add_index(path[x-1], path[x-2], new)
                        x -= 1
                # 如果运气很好的所有层都新增了索引，那么在最上层再roll一次新增一层
                if abs(x) == len(path)-1:
                    if random.randint(0, 1) == 1:
                        self._add_new_level(path[0], new)
                    else:
                        pass
                else:
                    pass
                return node.right

    def erase(self, val):
        # 删除需要从索引开始一层一层的删
        locate = self._locate(val)
        node = locate.node
        if node.right is None or node.right.val != val:
            return False
        path = locate.path
        # 从上到下的删除，
        for node in path:
            if node.right is not None and node.right.val == val:
                # node.right是需要删除的，先把node.right的down和right断开
                # 然后吧 right 链接到 node.right
                right = node.right.right
                node.right.down = None
                node.right.right = None
                node.right = right
        return True


a = SkipList()
for i in range(20):
    print(a.add((i + 1) * 10).val)
