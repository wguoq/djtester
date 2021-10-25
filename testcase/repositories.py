from djtester.base_repositories import BaseDBHelper
from testcase.models import Identity, Action, TestData, Check_Point

MODELS_PATH = 'testcase.models'


class TcIdentityDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Identity.__name__)

    def _save_m2m(self, new_model):
        return new_model

    def has_case_id(self, data):
        test_case_id = data.get('test_case_id')
        if super().filter_by({"test_case_id": test_case_id}).exists():
            return True
        else:
            return False

    def has_case_name(self, data):
        test_case_name = data.get('test_case_name')
        if super().filter_by({"test_case_name": test_case_name}).exists():
            return True
        else:
            return False


class TcActionDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Action.__name__)

    def _save_m2m(self, new_model):
        return new_model


class TcDataDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, TestData.__name__)

    def _save_m2m(self, new_model):
        return new_model


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self):
        super().__init__(MODELS_PATH, Check_Point.__name__)

    def _save_m2m(self, new_model):
        return new_model
