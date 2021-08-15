"""
testcase领域聚合
"""
from django.db import transaction
from testcase.domain.tc_model import TcTestCase
from testcase.models import Test_Case
from testcase.repositories import *


class TestCaseSDBHelper:
    def __init__(self, tc_testcase: TcTestCase):
        self.tc_testcase = tc_testcase
        self.testcaseS = Test_Case()
        self.testcaseS.pk = self.tc_testcase.id
        self.testcaseS.test_case_type = self.tc_testcase.test_case_type

    def _save_foreignkey(self):
        # tc_identity
        if self.tc_testcase.tc_identity:
            self.testcaseS.tc_identity = TestCaseIdentityDBHelper(
                Tc_Identity(**self.tc_testcase.tc_identity)).save_this_one()
        # tc_action
        if self.tc_testcase.tc_action:
            self.testcaseS.tc_action = TestCaseActionDBHelper(
                Tc_Action(**self.tc_testcase.tc_action)).save_this()
        # tc_data
        if self.tc_testcase.tc_data:
            self.testcaseS.tc_data = TestCaseDataDBHelper(Tc_Data(**self.tc_testcase.tc_data)).save_this()

    def _save_m2m(self):
        new_check_list = []
        if self.tc_testcase.tc_check_list:
            for check in self.tc_testcase.tc_check_list:
                if check:
                    new_check = TestCaseCheckPointDBHelper(Tc_Check_Point(**check)).save_this()
                    new_check_list.append(new_check)
        self.testcaseS.tc_check_list.add(*new_check_list)

    # m2m的字段要单独处理
    # 需要事务 官方文档把事务写在view层 todo
    @transaction.atomic
    def save_this_one(self):
        # 通过pk判断是否新增
        if self.testcaseS.pk:
            # 修改
            # 判断有没有这条数据
            # 查出数据修改
            try:
                self.testcaseS = self.get_by_pk(self.testcaseS.pk)
            except Exception as e:
                raise Exception(f'TestCaseIdentity get id为 {self.testcaseS.pk} 的数据报错: {e}')
            # 先写1对1外键
            self._save_foreignkey()
            # 保存1对1外键的
            self.testcaseS.save(update_fields=self.testcaseS.update_fields())
            # 保存m2m
            self._save_m2m()
            return self.testcaseS

        else:
            # 新增
            self._save_foreignkey()
            self.testcaseS.save()
            self._save_m2m()
            return self.testcaseS

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
        ide = TestCaseIdentityDBHelper(Tc_Identity()).get_by(dict(test_case_id=test_case_id))
        return Test_Case.objects.filter(tc_identity=ide)

    @staticmethod
    def filter_by_case_name(test_case_name):
        # test_case_name在TcIdentity表里面是唯一的可以用get
        ide = TestCaseIdentityDBHelper(Tc_Identity()).get_by(dict(test_case_name=test_case_name))
        return Test_Case.objects.filter(tc_identity=ide)
