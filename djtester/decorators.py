import inspect
from functools import wraps


class show_class_name(object):
    def __init__(self, msg='msg'):
        self.msg = msg

    def __call__(self, func):
        @wraps(func)
        def wrapped_function(*args, **kwargs):
            return func(*args, **kwargs)
        ss = inspect.stack()
        print(self.msg)
        # print(ss[1][0])
        # print(ss[1][1])
        # print(ss[1][2])
        print(f'class_name = {ss[1][3]}')
        # print(f'func_name = {ss[1][4]}')
        # print(ss[1][5])
        return wrapped_function

