import abc
import importlib
from django.db import transaction


def save_foreignkey(repositories_path, repositories_name, data):
    models_ = importlib.import_module(repositories_path)
    model = getattr(models_, repositories_name)
    if data is None:
        return None
    # 如果传入的是dict就保存
    elif isinstance(data, dict):
        return model(data).save_this()
    # 如果不是dict就假设是pk去查询
    else:
        return model().get_by(dict(pk=data))


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str):
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)

    def get_all(self, offset: int, limit: int):
        return self.model.objects.all()[offset: (offset + limit)]

    def get_by(self, kwargs):
        return self.model.objects.get(**kwargs)

    def filter_by(self, kwargs):
        return self.model.objects.filter(**kwargs)

    @abc.abstractmethod
    def _save_m2m_func(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        """
        如果model里有外键,data里面要保证对应字段是正确的obj,
        如果有m2m的外键,要把data里对应字段移出去,
        否则self.model(**data)就会报错
        """
        pk = self.model(**data).pk
        if pk:
            # todo 写入修改时间
            self.model.objects.filter(pk=pk).update(**data)
            new = self.model.objects.get(pk=pk)
            return self._save_m2m_func(new)
        else:
            # todo 写入创建时间
            new = self.model(**data).save()
            return self._save_m2m_func(new)


