# 应该弄成yml文件
from enum import Enum


class CheckOperator(Enum):
    EQ = 'equals'
    NE = 'not equals'


class ResponseProperty(Enum):
    STATUS_CODE = 'status_code'
    HEADERS = 'headers'
    COOKIES = 'cookies'
    JSON = 'json'
