import importlib
from django.db import models

from testcase.models import Tc_Identity, Tc_Action, Tc_Data, Tc_Check_Point


class BaseDBHelper:
    def __init__(self, model: models):
        self.model = model
        # 由于model.objects必须用本来的model来调用,所以只能import对应的model
        testcase_models = importlib.import_module('testcase.models')
        self.entity = getattr(testcase_models, self.model.__class__.__name__)

    def save_this(self):
        # 通过pk判断是否新增
        if self.model.pk:
            # 查出数据后修改
            try:
                self.model = self.entity.objects.get(pk=self.model.pk)
            except Exception as e:
                raise Exception(f'{self.model.__class__.__name__} get id为 {self.model.pk} 的数据报错: {e}')
            self.model.save(update_fields=self.model.update_fields())
            return self.model
        else:
            # 新增
            self.model.save()
            return self.model

    def get_all(self, offset: int, limit: int):
        return self.entity.objects.all()[offset: (offset + limit)]

    def get_by(self, kwargs):
        return self.entity.objects.get(**kwargs)

    def filter_by(self, kwargs):
        return self.entity.objects.filter(**kwargs)


class TcIdentityDBHelper(BaseDBHelper):
    def __init__(self, tc_identity: Tc_Identity):
        super().__init__(tc_identity)
        self.identity = tc_identity

    def has_case_id(self, test_case_id):
        if super().filter_by({"test_case_id": test_case_id}).exists():
            return True
        else:
            return False

    def has_case_name(self, test_case_name):
        if super().filter_by({"test_case_name": test_case_name}).exists():
            return True
        else:
            return False

    def save_this_one(self):
        # 无论新增还是修改都要判断id和name是否重复
        if self.has_case_id(self.identity.test_case_id):
            raise Exception(f'test_case_id {self.identity.test_case_id} 已存在')
        elif self.has_case_name(self.identity.test_case_name):
            raise Exception(f'test_case_name {self.identity.test_case_name} 已存在')
        else:
            return super().save_this()


class TcActionDBHelper(BaseDBHelper):
    def __init__(self, tc_action: Tc_Action):
        super().__init__(tc_action)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self, tc_data: Tc_Data):
        super().__init__(tc_data)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self, tc_check_point: Tc_Check_Point):
        super().__init__(tc_check_point)
