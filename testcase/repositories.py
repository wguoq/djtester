import importlib
from django.db import transaction

from testcase.models import Tc_Identity, Tc_Action, Tc_Data, Tc_Check_Point


class BaseDBHelper:
    def __init__(self, model_name: str, data: dict = None):
        # 由于model.objects必须用本来的model来调用,所以只能import对应的model
        testcase_models = importlib.import_module('testcase.models')
        self.model = getattr(testcase_models, model_name)
        if data:
            self.entity = self.model(**data)

    @transaction.atomic
    def save_this(self):
        # 通过pk判断是否新增
        if self.entity.pk:
            # 有pk判断为修改已存在的数据
            try:
                self.entity = self.model.objects.get(pk=self.entity.pk)
            except Exception as e:
                raise Exception(f'从 {self.model.__name__} 查询 id为 {self.entity.pk} 的数据报错: {e}')
            # 必须先取出不为空的字段,再传入修改,还要排除id字段
            entity_dict: dict = self.entity.fields_dict()
            for (k, v) in entity_dict.items():
                if v is None or k in ['id']:
                    entity_dict.pop(k)
                else:
                    continue
            self.entity.save(update_fields=entity_dict.keys())
            return self.entity
        else:
            # 没有pk判断为新增
            self.entity.save()
            return self.entity

    def get_all(self, offset: int, limit: int):
        return self.model.objects.all()[offset: (offset + limit)]

    def get_by(self, kwargs):
        return self.model.objects.get(**kwargs)

    def filter_by(self, kwargs):
        return self.model.objects.filter(**kwargs)


class TcIdentityDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        self.data = data
        super().__init__(Tc_Identity.__name__, data)

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

    @transaction.atomic
    def save_this(self):
        # 无论新增还是修改都要判断id和name是否重复
        test_case_id = self.data.get('test_case_id')
        test_case_name = self.data.get('test_case_name')
        if self.has_case_id(test_case_id):
            raise Exception(f'test_case_id {test_case_id} 已存在')
        elif self.has_case_name(test_case_name):
            raise Exception(f'test_case_name {test_case_name} 已存在')
        else:
            return super().save_this()


class TcActionDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(Tc_Action.__name__, data)


class TcDataDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(Tc_Data.__name__, data)


class TcCheckPointDBHelper(BaseDBHelper):
    def __init__(self, data: dict = None):
        super().__init__(Tc_Check_Point.__name__, data)
