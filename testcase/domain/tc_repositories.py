from testcase.models import Test_Case
from testcase.repositories import *
MODELS_PATH = 'testcase.models'


class TcTestCaseDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(MODELS_PATH, Test_Case.__name__, None)
        self.data = data

    def _save_foreignkey(self):
        tc_identity = self.data.get('tc_identity')
        tc_action = self.data.get('tc_action')
        tc_data = self.data.get('tc_data')
        if tc_identity:
            self.data['tc_identity'] = TcIdentityDBHelper(tc_identity).save_this()
        if tc_action:
            self.data['tc_action'] = TcActionDBHelper(tc_action).save_this()
        if tc_data:
            self.data['tc_data'] = TcDataDBHelper(tc_data).save_this()
        return self.data

    def _save_m2m(self):
        new_check_list = []
        if self.data.get('tc_check_list'):
            for check in self.data.get('tc_check_list'):
                if check:
                    new_check = TcCheckPointDBHelper(check).save_this()
                    new_check_list.append(new_check)
            return new_check_list
        else:
            return None

    # m2m的字段要单独处理
    # 需要事务
    @transaction.atomic
    def save_this(self):
        # 通过pk判断是否新增
        pk = self.data.get('id')
        if pk:
            # 先写外键
            foreignkey_data = self._save_foreignkey()
            m2m = self._save_m2m()
            new = Test_Case.objects.update(**foreignkey_data)
            obj = Test_Case.objects.get(pk=new)
            if m2m:
                obj.tc_check_list.add(*m2m)
            return obj
        else:
            # 新增外键
            foreignkey_data = self._save_foreignkey()
            m2m = self._save_m2m()
            # 移除m2m字段
            if "tc_check_list" in foreignkey_data.keys():
                foreignkey_data.pop('tc_check_list')
            # 保存
            new = Test_Case(**foreignkey_data)
            new.save()
            # 保存m2m
            if m2m:
                new.tc_check_list.add(*m2m)
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
