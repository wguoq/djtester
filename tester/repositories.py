from django.db import transaction

from djtester.repositories import BaseDBHelper, save_foreignkey
from .models import *

MODELS_PATH = 'tester.models'
REPOSITORIES_PATH = 'tester.repositories'


class TestCaseDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Test_Case.__name__)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Tc_Data.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['test_case'] = save_foreignkey(REPOSITORIES_PATH, TestCaseDBHelper.__name__, data.get('test_case'))
        return super().save_this(data)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Tc_CheckPoint.__name__)

    @transaction.atomic
    def save_this(self, data: dict):
        data['tc_data'] = save_foreignkey(REPOSITORIES_PATH, TcDataDBHelper.__name__, data.get('tc_data'))
        return super().save_this(data)
