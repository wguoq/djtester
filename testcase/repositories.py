from django.db import transaction
from djtester.base_repositories import BaseDBHelper
from testcase.models import Tc_Identity, Tc_Action, Tc_Data, Tc_Check_Point

MODELS_PATH = 'testcase.models'


class TcIdentityDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        self.data = data
        super().__init__(MODELS_PATH, Tc_Identity.__name__, data)

    def has_case_id(self):
        test_case_id = self.data.get('test_case_id')
        if super().filter_by({"test_case_id": test_case_id}).exists():
            return True
        else:
            return False

    def has_case_name(self):
        test_case_name = self.data.get('test_case_name')
        if super().filter_by({"test_case_name": test_case_name}).exists():
            return True
        else:
            return False


class TcActionDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(MODELS_PATH, Tc_Action.__name__, data)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(MODELS_PATH, Tc_Data.__name__, data)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(MODELS_PATH, Tc_Check_Point.__name__, data)
