from djtester.repositories import BaseDBHelper
from testcase.models import Action, TestData, Check_Point

MODELS_PATH = 'testcase.models'


class TcActionDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Action.__name__)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, TestData.__name__)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Check_Point.__name__)
