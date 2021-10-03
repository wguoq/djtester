import importlib
from django.db import transaction


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str, data: dict = None):
        self.data = data
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)
        # self.entity = self.model()
        # if self.data:
        #     self.entity = self.model(**self.data)

    def get_all(self, offset: int, limit: int):
        return self.model.objects.all()[offset: (offset + limit)]

    def get_by(self, kwargs):
        return self.model.objects.get(**kwargs)

    def filter_by(self, kwargs):
        return self.model.objects.filter(**kwargs)

    @transaction.atomic
    def save_this(self):
        entity = self.model(**self.data)
        pk = entity.pk
        if pk:
            # todo 写入修改时间
            self.model.objects.filter(pk=pk).update(**self.data)
            return self.model.objects.get(pk=pk)
        else:
            # todo 写入创建时间
            entity.save()
            return entity


