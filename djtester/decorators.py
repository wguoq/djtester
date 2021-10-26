import inspect
import os
from functools import wraps
from ruamel import yaml


class show_class_name(object):
    def __init__(self, msg=''):
        self.msg = msg

    def __call__(self, func):
        stack = inspect.stack()
        print(self.msg)
        # print(stack[1][0])
        # print(f'file_path = {stack[1][1]}')
        # print(stack[1][2])
        print(f'class_name = {stack[1][3]}')
        # print(f'func_name = {stack[1][4]}')
        # print(stack[1][5])

        @wraps(func)
        def wrapped_function(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapped_function


cur_path = os.path.dirname(os.path.realpath(__file__))
node_func_list = os.path.join(cur_path, "node_func_list.yaml")


class reg_node_func(object):
    def __init__(self, node_type, class_path):
        self.node_type = node_type
        self.class_path = class_path

    def __call__(self, func):
        stack = inspect.stack()
        class_name = stack[1][3]
        # 先把所有内容dict读出来,把新的update进去
        file = open(node_func_list, 'r', encoding="utf-8")
        read = file.read()
        file.close()

        data: dict = yaml.load(read, Loader=yaml.Loader)
        if data is None:
            data = {}

        data.update({self.node_type: {'class_path': self.class_path,
                                      'class_name': class_name}})

        with open(node_func_list, "w", encoding="utf-8") as f:
            yaml.dump(data, f, Dumper=yaml.RoundTripDumper)

        @wraps(func)
        def wrapped_function(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapped_function
