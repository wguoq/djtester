import importlib
from django.db import transaction


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str, data: dict = None):
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)
        self.entity = self.model()
        if data:
            self.entity = self.model(**data)
        # 取出所有field的name以后用
        self.fields_name = []
        for f in self.entity._meta.fields:
            self.fields_name.append(f.name)

    @transaction.atomic
    def save_this(self):
        # 通过pk判断是否新增
        if self.entity.pk:
            # 有pk判断为修改已存在的数据
            # 修改前需要获取正确的update_fields
            update_fields = self._get_update_fields()
            self.entity.save(update_fields=update_fields)
            return self.entity
        else:
            # 没有pk判断为新增
            self.entity.save()
            return self.entity

    def _get_update_fields(self):
        # 1,排除值为None的字段
        # 2,排除id字段
        entity_dict: dict = self.entity.__dict__
        for (k, v) in entity_dict.items():
            if v is None:
                self.fields_name.remove(k)
            else:
                continue
        if 'id' in self.fields_name:
            self.fields_name.remove('id')
        return self.fields_name

    def get_all(self, offset: int, limit: int):
        return self.model.objects.all()[offset: (offset + limit)]

    def get_by(self, kwargs):
        return self.model.objects.get(**kwargs)

    def filter_by(self, kwargs):
        return self.model.objects.filter(**kwargs)

