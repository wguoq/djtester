"""
testcase领域聚合
"""
from django.db import transaction
from testcase.domain.tc_model import TcTestCase
from testcase.models import Test_Case
from testcase.repositories import *


class TcTestCaseDBHelper:
    def __init__(self, data: dict = None):
        self.data = data
        if self.data:
            self.test_case = Test_Case()
            self.test_case.pk = self.data.get('id')
            self.test_case.test_case_type = self.data.get('test_case_type')

    def _save_foreignkey(self):
        tc_identity = self.data.get('tc_identity')
        tc_action = self.data.get('tc_action')
        tc_data = self.data.get('tc_data')
        if tc_identity:
            self.test_case.tc_identity = TcIdentityDBHelper(tc_identity).save_this()
        if tc_action:
            self.test_case.tc_action = TcActionDBHelper(tc_action).save_this()
        if tc_data:
            self.test_case.tc_data = TcDataDBHelper(tc_data).save_this()

    def _save_m2m(self):
        new_check_list = []
        if self.data.get('tc_check_list'):
            for check in self.data.get('tc_check_list'):
                if check:
                    new_check = TcCheckPointDBHelper(check).save_this()
                    new_check_list.append(new_check)
        return new_check_list

    # m2m的字段要单独处理
    # 需要事务
    @transaction.atomic
    def save_this(self):
        # 通过pk判断是否新增
        pk = self.test_case.pk
        if pk:
            # 查出数据修改
            try:
                self.test_case = self.get_by_pk(pk)
            except Exception as e:
                raise Exception(f'Test_Case表 查询 id为 {pk} 的数据报错: {e}')

            # 先写1对1外键
            self._save_foreignkey()

            # 保存Test_Case里有数据的字段但是要排除id,tc_check_list字段
            test_case_dict: dict = self.test_case.fields_dict()
            update_fields = []
            for (k, v) in test_case_dict.items():
                if v is None or k in ['id', 'tc_check_list']:
                    continue
                else:
                    update_fields.append(k)
            self.test_case.save(update_fields=update_fields)

            # 保存m2m
            self.test_case.tc_check_list.add(*self._save_m2m())
            return self.test_case

        else:
            # 新增
            self._save_foreignkey()
            self.test_case.save()
            self.test_case.tc_check_list.add(*self._save_m2m())
            return self.test_case

    @staticmethod
    def get_all(offset: int, limit: int):
        return Test_Case.objects.all()[offset: (offset + limit)]

    @staticmethod
    def get_by_pk(pk):
        return Test_Case.objects.get(pk=pk)

    @staticmethod
    def filter_by(kwargs: dict):
        return Test_Case.objects.filter(**kwargs)

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
