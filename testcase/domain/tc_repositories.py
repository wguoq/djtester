import importlib

from django.db import transaction

from djtester.base_repositories import BaseDBHelper
from testcase.models import Test_Case
from testcase.repositories import TcIdentityDBHelper

MODELS_PATH = 'testcase.models'

helper = {'tc_identity': 'TcIdentityDBHelper',
          'tc_action': 'TcActionDBHelper',
          'tc_data': 'TcDataDBHelper',
          'tc_check_point': 'TcCheckPointDBHelper'}


def _save_foreignkey(foreignkey_name, data):
    models = importlib.import_module('testcase.repositories')
    model_name = helper.get(foreignkey_name)
    model = getattr(models, model_name)
    if data is None:
        return None
    # 如果传入的是dict就保存
    elif isinstance(data, dict):
        return model(data).save_this()
    # 如果不是dict就假设是pk去查询
    else:
        return model().get_by(dict(pk=data))


class TcTestCaseDBHelper(BaseDBHelper):
    """
    data里面的外键字段可以是dict也可以是id
    """
    def __init__(self, data: dict = None):
        super().__init__(MODELS_PATH, Test_Case.__name__, None)
        if data:
            self.data = data
            self.tc_identity = self.data.get('tc_identity')
            self.tc_action = self.data.get('tc_action')
            self.tc_data = self.data.get('tc_data')
            self.tc_check_list = self.data.get('tc_check_list')
            # tc_check_list是m2m要先移除
            if self.tc_check_list:
                self.data.pop('tc_check_list')
        else:
            self.data = {}

    def _set_foreignkey(self):
        self.data['tc_identity'] = _save_foreignkey('tc_identity', self.tc_identity)
        self.data['tc_action'] = _save_foreignkey('tc_action', self.tc_action)
        self.data['tc_data'] = _save_foreignkey('tc_data', self.tc_data)

    def _set_m2m(self):
        if self.tc_check_list is None:
            return None
        elif len(self.tc_check_list) == 0:
            return []
        elif isinstance(self.tc_check_list, list):
            new_check_list = []
            for check in self.tc_check_list:
                a = _save_foreignkey('tc_check_point', check)
                new_check_list.append(a)
            return new_check_list
        else:
            raise Exception(f'tc_check_list 需要是None或者list类型')

    # m2m的字段要单独处理
    # 事务
    @transaction.atomic
    def save_this(self):
        # 存1:1外键
        self._set_foreignkey()
        # 存m2m
        m2m = self._set_m2m()
        # 通过pk判断是不是修改
        pk = self.data.get('id')
        if pk:
            # 修改前先排除值为None的字段
            new_data = {}
            for k, v in self.data.items():
                if v:
                    new_data.update({k: v})
            Test_Case.objects.filter(pk=pk).update(**new_data)
            new = Test_Case.objects.get(pk=pk)
            if m2m:
                new.tc_check_list.set(m2m)
            return new
        else:
            # 新增
            new = Test_Case(**self.data)
            new.save()
            if m2m:
                new.tc_check_list.set(m2m)
            return new

    @staticmethod
    def filter_by_case_id(test_case_id):
        tc_identity_id = TcIdentityDBHelper().get_by(dict(test_case_id=test_case_id)).pk
        return Test_Case.objects.filter(tc_identity=tc_identity_id)

    @staticmethod
    def filter_by_case_name(test_case_name):
        tc_identity_id = TcIdentityDBHelper().get_by(dict(test_case_name=test_case_name)).pk
        return Test_Case.objects.filter(tc_identity=tc_identity_id)
