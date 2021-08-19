import importlib
from django.db import transaction


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str, data: dict = None):
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)
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

