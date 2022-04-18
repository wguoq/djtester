import importlib
import operator
import sys

# 显示内存地址
from operator import itemgetter

import requests

a = 1
print(id(a))

# 基本类型
int_a = 1
float_a = 0.1
complex_a = 0.1j
str_a = 'str_a'

# \转义,r取消转义
da = 'aa\n'
da2 = 'aa\\n'
da3 = r'aa\n'
print(da)
print(da2)
print(da3)

# range步长
for i in range(2, 101, 2):
    print(i)

# list方法
l = ['1', '2', '3']
x = '5'
l.append(x)
l.insert(1, x)
l.remove(x)  # 删除值=x的
l.pop(1)  # 删除第i个else最后一个
l.sort()  # 排序
l.reverse()  # 反转
l.clear()
# 元组()
# 元组内有list时,是可以改变这个list的内容
(1, 2, 3, l)

# 集合
set()
a = {1, 2, 3}
b = {3, 4, 5}

a.union(b)
a.intersection(b)
a.difference(b)

# 字典
a = {}
b = a.fromkeys(('a', 'b', 'c'), '1')
print(b)
# 字典推导式
x = {i: i + 1 for i in range(4)}
print(x)
# dir()列出当前导入的所有方法和变量
print(dir())
print(dir(sys))

# *list 解包 **dict 解包
ll = ['1', '2', '3']
print('ll is {}+{}+{}'.format(*ll))
dd = {'a': 1,
      'b': 2,
      'c': 3}
print('dd is {a}+{b}+{c}'.format(**dd))

# f标志在''里面自动格式化读取变量和lambda和def,不需要解包,不支持转义符\
name1 = 'abcd'
name2 = f'1234+{name1}'
name3 = f'1234+{name1[0]}'
name4 = f'1234+{(lambda x: x + 1)(1)}'


def fff(x):
    return x + 1


name5 = f'1234+{fff(3)}'
print(name2)
print(name3)
print(name4)
print(name5)


# python中动态加载模块和类方法实现测试代码
#
# 文件名： mytest.py
# 具体代码如下：
#
# 注意：模块名，类名，方法名都是变量。

# coding=UTF-8

class TestClass:
    def sub(self, a, b):
        return a - b

    def add(self, a, b):
        return a + b

    def echo(self):
        print
        "test"


def main():
    class_name = "TestClass"  # 类名
    module_name = "mytest"  # 模块名
    method = "echo"  # 方法名

    module = importlib.import_module(module_name)  # import module
    print
    "#module:", module
    c = getattr(module, class_name)
    print
    "#c:", c
    obj = c()  # new class
    print
    "#obj:", obj
    print(obj)
    obj.echo()
    mtd = getattr(obj, method)
    print
    "#mtd:", mtd
    mtd()  # call def

    mtd_add = getattr(obj, "add")
    t = mtd_add(1, 2)
    print
    "#t:", t

    mtd_sub = getattr(obj, "sub")
    print
    mtd_sub(2, 1)


getter = itemgetter("status_code")

a = requests.get('https://www.baidu.com/')

print(a.status_code)
a = '500'
b = '200'
print(operator.gt(a, b))

# python 3.10
# Result = Ok | Err
#
#
# def abcd(value: str) -> Result:
#     if value.isnumeric():
#         return Ok(int(value))
#     else:
#         return Err(f'{value} is not numeric')
#
#
# match abcd('1234'):
#     case Ok(value):
#         print(f'Result is Ok value is {value}')
#     case Err(message):
#         print(f'Result is Err value is {message}')

lll = [1, 2, 3, 4, 5, 6, 7, 8]

lll.reverse()
print(lll)

a = 123
b = '-asd113'
print(a/10)
print(int(b))


a = "a"
b = a or None
print(b)

a = 0
b = a or None
print(b)
