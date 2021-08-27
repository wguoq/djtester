import importlib

from testcase.models import Test_Case
from testcase.repositories import *
MODELS_PATH = 'testcase.models'

helper = {'tc_identity': 'TcIdentityDBHelper',
          'tc_action': 'TcActionDBHelper',
          'tc_data': 'TcDataDBHelper'}


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
    data可以是有字段数据的dict也可也是已存在数据的id
    """
    def __init__(self, data: dict = None):
        super().__init__(MODELS_PATH, Test_Case.__name__, None)
        if data:
            self.data = data
            self.tc_identity = self.data.get('tc_identity')
            self.tc_action = self.data.get('tc_action')
            self.tc_data = self.data.get('tc_data')
            self.tc_check_list = self.data.get('tc_check_list')
            if self.tc_check_list:
                self.data.pop('tc_check_list')
        else:
            self.data = {}

    def _set_foreignkey(self):
        # 需要处理数据是dict或者id的情况
        # if self.tc_identity is None:
        #     self.data['tc_identity'] = None
        # elif isinstance(self.tc_identity, dict):
        #     self.data['tc_identity'] = TcIdentityDBHelper(self.tc_identity).save_this()
        # else:
        #     self.data['tc_identity'] = TcIdentityDBHelper().get_by(dict(pk=self.tc_identity))
        #
        # if self.tc_action is None:
        #     self.data['tc_action'] = None
        # elif isinstance(self.tc_action, dict):
        #     self.data['tc_action'] = TcActionDBHelper(self.tc_action).save_this()
        # else:
        #     self.data['tc_action'] = TcActionDBHelper().get_by(dict(pk=self.tc_action))
        #
        # if self.tc_data is None:
        #     self.data['tc_data'] = None
        # elif isinstance(self.tc_data, dict):
        #     self.data['tc_data'] = TcDataDBHelper(self.tc_data).save_this()
        # else:
        #     self.data['tc_data'] = TcDataDBHelper().get_by(dict(pk=self.tc_data))
        self.data['tc_identity'] = _save_foreignkey('tc_identity', self.tc_identity)
        self.data['tc_action'] = _save_foreignkey('tc_action', self.tc_action)
        self.data['tc_data'] = _save_foreignkey('tc_data', self.tc_data)

        return self.data

    def _set_m2m(self):
        new_check_list = []
        if self.tc_check_list is None:
            return None
        elif len(self.tc_check_list) == 0:
            return new_check_list
        else:
            for check in self.tc_check_list:
                if isinstance(check, dict):
                    new_check = TcCheckPointDBHelper(check).save_this()
                    new_check_list.append(new_check)
                else:
                    new_check = TcCheckPointDBHelper().get_by(dict(pk=check))
                    new_check_list.append(new_check)
            return new_check_list

    # m2m的字段要单独处理
    # 事务
    @transaction.atomic
    def save_this(self):
        # 存1:1外键
        self._set_foreignkey()
        # 存m2m
        m2m = self._set_m2m()
        # 通过pk判断是否新增
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
            new = Test_Case(**self.data)
            new.save()
            if m2m:
                new.tc_check_list.set(m2m)
            return new

    @staticmethod
    def filter_by_case_id(test_case_id):
        ide = TcIdentityDBHelper().get_by(dict(test_case_id=test_case_id))
        return Test_Case.objects.filter(tc_identity=ide)

    @staticmethod
    def filter_by_case_name(test_case_name):
        ide = TcIdentityDBHelper().get_by(dict(test_case_name=test_case_name))
        return Test_Case.objects.filter(tc_identity=ide)
