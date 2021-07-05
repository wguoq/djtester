from enum import Enum


# Enum枚举类型
class CaseType(Enum):
    API = 'api'
    UI = 'ui'
    MOBILE = 'mobile'


class MobileMethod(Enum):
    CLICK = 'click'
    INPUT = 'input'


class UiMethod(Enum):
    CLICK = 'click'
    INPUT = 'input'


class HttpMethod(Enum):
    GET = 'GET'
    POST = 'POST'
    PUT = 'PUT'
    DELETE = 'DELETE'
    HEAD = 'HEAD'
    OPTIONS = 'OPTIONS'
    PATCH = 'PATCH'


class HttpProtocol(Enum):
    HTTP = 'http'
    HTTPS = 'https'


class CheckOperator(Enum):
    EQ = 'equals'


class ResponseProperty(Enum):
    STATUS_CODE = 'status_code'
    HEADERS = 'headers'
    COOKIES = 'cookies'
    JSON = 'json'
