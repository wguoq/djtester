import random
import time
from djtester.repositories import BaseRepository
from .apps import TestcaseConfig
from .models import *


class TestCaseRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, TestCase.__name__)

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
        super().__init__(TestcaseConfig.name, TcApi.__name__)


class TcApiDataRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, TcApiData.__name__)


class TcDataRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, TcData.__name__)


class TcCheckPointRepository(BaseRepository):
    def __init__(self):
        super().__init__(TestcaseConfig.name, TcCheckPoint.__name__)
