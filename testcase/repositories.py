from django.db import transaction

from djtester.repositories import BaseDBHelper, save_foreignkey
from testcase.models import Action, TestData, Check_Point, Test_Case

MODELS_PATH = 'testcase.models'
REPOSITORIES_PATH = 'testcase.repositories'


class TcActionDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Action.__name__)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, TestData.__name__)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Check_Point.__name__)


class TestCaseDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Test_Case.__name__)
        self.m2m = None

    @staticmethod
    def _save_m2m(tc_check_list):
        if tc_check_list is None:
            return None
        elif isinstance(tc_check_list, list):
            new_check_list = []
            for check in tc_check_list:
                a = save_foreignkey(REPOSITORIES_PATH, TcCheckPointDBHelper.__name__, check)
                new_check_list.append(a)
            return new_check_list
        else:
            return None

    @transaction.atomic
    def save_this(self, data: dict):
        # 把外键拿出来
        tc_action = data.get('tc_action')
        tc_data = data.get('tc_data')
        tc_check_list = data.get('tc_check_list')
        # 存外键
        if tc_action:
            data['tc_action'] = save_foreignkey(REPOSITORIES_PATH, TcActionDBHelper.__name__, tc_action)
        else:
            pass
        if tc_data:
            data['tc_data'] = save_foreignkey(REPOSITORIES_PATH, TcDataDBHelper.__name__, tc_data)
        else:
            pass
        # 存之前要把m2m字段移除
        if tc_check_list:
            data.pop('tc_check_list')
            new_model = super().save_this(data)
            self.m2m = self._save_m2m(tc_check_list)
            new_model.tc_check_list.set(self.m2m)
            return new_model
        else:
            return super().save_this(data)
