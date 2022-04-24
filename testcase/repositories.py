import random
import time
from djtester.repositories import BaseRepository
from .models import *

APP_NAME = 'testcase'


class TestCaseRepository(BaseRepository):
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


class TcApiRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, TcApi.__name__)


class TcApiDataRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, TcApiData.__name__)


class TcDataRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, TcData.__name__)


class TcCheckPointRepository(BaseRepository):
    def __init__(self):
        super().__init__(APP_NAME, TcCheckPoint.__name__)
