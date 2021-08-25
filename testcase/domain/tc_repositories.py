from testcase.models import Test_Case
from testcase.repositories import *
MODELS_PATH = 'testcase.models'


class TcTestCaseDBHelper(BaseDBHelper):
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

    def _save_foreignkey(self):
        # 需要处理数据是dict或者id的情况
        if self.tc_identity is None:
            self.data['tc_identity'] = None
        elif isinstance(self.tc_identity, dict):
            self.data['tc_identity'] = TcIdentityDBHelper(self.tc_identity).save_this()
        else:
            self.data['tc_identity'] = TcIdentityDBHelper().get_by(dict(pk=self.tc_identity))

        if self.tc_action is None:
            self.data['tc_action'] = None
        elif isinstance(self.tc_action, dict):
            self.data['tc_action'] = TcActionDBHelper(self.tc_action).save_this()
        else:
            self.data['tc_action'] = TcActionDBHelper().get_by(dict(pk=self.tc_action))

        if self.tc_data is None:
            self.data['tc_data'] = None
        elif isinstance(self.tc_data, dict):
            self.data['tc_data'] = TcDataDBHelper(self.tc_data).save_this()
        else:
            self.data['tc_data'] = TcDataDBHelper().get_by(dict(pk=self.tc_data))

        return self.data

    def _save_m2m(self):
        new_check_list = []
        if self.tc_check_list:
            for check in self.tc_check_list:
                if isinstance(check, dict):
                    new_check = TcCheckPointDBHelper(check).save_this()
                    new_check_list.append(new_check)
                else:
                    new_check = TcCheckPointDBHelper().get_by(dict(pk=check))
                    new_check_list.append(new_check)
            return new_check_list
        else:
            return None

    # m2m的字段要单独处理
    # 需要事务
    @transaction.atomic
    def save_this(self):
        # 存1:1外键
        self._save_foreignkey()
        # 存m2m
        m2m = self._save_m2m()
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
        # test_case_id在TcIdentity表里面是唯一的可以用get
        ide = TcIdentityDBHelper().get_by(dict(test_case_id=test_case_id))
        return Test_Case.objects.filter(tc_identity=ide)

    @staticmethod
    def filter_by_case_name(test_case_name):
        # test_case_name在TcIdentity表里面是唯一的可以用get
        ide = TcIdentityDBHelper().get_by(dict(test_case_name=test_case_name))
        return Test_Case.objects.filter(tc_identity=ide)
