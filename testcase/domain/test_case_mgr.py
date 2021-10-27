from django.db import transaction
from djtester.repositories import save_foreignkey
from testcase.models import Test_Case
from testcase.repositories import *

MODELS_PATH = 'testcase.models'
REPOSITORIES_PATH = 'testcase.repositories'


class TestCaseDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Test_Case.__name__)

    @staticmethod
    def _set_m2m(tc_check_list):
        if tc_check_list is None:
            return None
        elif len(tc_check_list) == 0:
            return []
        elif isinstance(tc_check_list, list):
            new_check_list = []
            for check in tc_check_list:
                a = save_foreignkey(REPOSITORIES_PATH, TcCheckPointDBHelper.__name__, check)
                new_check_list.append(a)
            return new_check_list
        else:
            raise Exception(f'tc_check_list 需要是list或者None')

    def _save_m2m(self, new_model):
        if self.m2m:
            new_model.tc_check_list.set(self.m2m)
            return new_model
        else:
            return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        tc_identity = data.get('tc_identity')
        tc_action = data.get('tc_action')
        tc_data = data.get('tc_data')
        tc_check_list = data.get('tc_check_list')
        # tc_check_list是m2m要先移除
        if tc_check_list:
            data.pop('tc_check_list')
        # 存外键
        if tc_identity:
            data['tc_identity'] = save_foreignkey(REPOSITORIES_PATH, TcIdentityDBHelper.__name__, tc_identity)
        if tc_action:
            data['tc_action'] = save_foreignkey(REPOSITORIES_PATH, TcActionDBHelper.__name__, tc_action)
        if tc_data:
            data['tc_data'] = save_foreignkey(REPOSITORIES_PATH, TcDataDBHelper.__name__, tc_data)
        # 存m2m
        self.m2m = self._set_m2m(tc_check_list)
        return super().save_this(data)

    @staticmethod
    def filter_by_case_id(test_case_id):
        tc_identity_id = TcIdentityDBHelper().get_by(dict(test_case_id=test_case_id)).pk
        return Test_Case.objects.filter(tc_identity=tc_identity_id)

    @staticmethod
    def filter_by_case_name(test_case_name):
        tc_identity_id = TcIdentityDBHelper().get_by(dict(test_case_name=test_case_name)).pk
        return Test_Case.objects.filter(tc_identity=tc_identity_id)
