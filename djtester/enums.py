from enum import Enum


class TestCaseType(Enum):
    API = 'api'
    UI = 'ui'
    MOBILE = 'mobile'


# class HttpMethod(Enum):
#     GET = 'GET'
#     POST = 'POST'
#     DELETE = 'DELETE'
#     OPTIONS = 'OPTIONS'


class HttpProtocol(Enum):
    HTTP = 'http'
    HTTPS = 'https'


class ResponseProperty(Enum):
    STATUS_CODE = 'status_code'
    HEADERS = 'headers'
    COOKIES = 'cookies'
    JSON = 'json'


class Operators(Enum):
    # a == b
    EQ = 'eq'
    # a != b
    NE = 'ne'
    # a < b
    LT = 'lt'
    # a <= b
    LE = 'le'
    # a >= b
    GE = 'ge'
    # a > b
    GT = 'gt'


class TestResult(Enum):
    PASS = 'pass'
    FAIL = 'fail'
    ERROR = 'error'
