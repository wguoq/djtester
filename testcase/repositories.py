import random
import time
from djtester.repositories import BaseDBHelper
from .models import *

APP_NAME = 'testcase'


class TestCaseDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, TestCase.__name__)

    def save_this(self, data: dict):
        code = data.get('code')
        if code is None or len(code) == 0:
            code = 'tc' + str(round(time.time()) + random.randint(0, 99))
            data.update({"code": code})
        else:
            pass
        return super().save_this(data)


class TcApiDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, TcApi.__name__)


class TcApiDataDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, TcApiData.__name__)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, TcData.__name__)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(APP_NAME, TcCheckPoint.__name__)
