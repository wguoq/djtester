import inspect
from functools import wraps


class logit(object):
    def __init__(self, logfile='out.log'):
        self.logfile = logfile

    def __call__(self, func):
        @wraps(func)
        def wrapped_function(*args, **kwargs):
            log_string = func.__name__ + " was called"
            print(log_string)
            # 打开logfile并写入
            with open(self.logfile, 'a') as opened_file:
                # 现在将日志打到指定的文件
                opened_file.write(log_string + '\n')
            # 现在，发送一个通知
            self.notify()
            return func(*args, **kwargs)

        return wrapped_function

    def notify(self):
        # logit只打日志，不做别的
        pass


class show_class_name(object):
    def __init__(self, msg=None):
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
        print(f'func_name = {ss[1][4]}')
        # print(ss[1][5])
        return wrapped_function


class asefde(object):
    @show_class_name('aaaaaa')
    def __init__(self):
        self.a = '12345'

    def print_a(self):
        print('111111111')


print(asefde().print_a())

