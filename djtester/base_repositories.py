import abc
import importlib
from django.db import transaction


def save_foreignkey(repositories_path, repositories_name, foreignkey_data):
    models_ = importlib.import_module(repositories_path)
    model = getattr(models_, repositories_name)
    if foreignkey_data is None:
        raise Exception(f'{foreignkey_data} foreignkey_data is None')
    # 如果传入的是dict就保存
    elif isinstance(foreignkey_data, dict):
        return model().save_this(foreignkey_data)
    # 如果是int或者str就认为是pk,去查询出来
    elif isinstance(foreignkey_data, int) or isinstance(foreignkey_data, str):
        return model().get_by(dict(pk=foreignkey_data))
    # 其他情况认为是obj不处理
    else:
        return foreignkey_data


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str):
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)
        self.m2m = None

    def get_all(self, offset: int = 0, limit: int = 1000):
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
        如果有m2m的外键,要把data里对应字段移出去,单独保存后set进来
        否则self.model(**data)就会报错
        """
        entity = self.model(**data)
        pk = entity.pk
        if pk:
            self.model.objects.filter(pk=pk).update(**data)
            new = self.model.objects.get(pk=pk)
            if self.m2m:
                return self._save_m2m_func(new)
            else:
                return new
        else:
            entity.save()
            if self.m2m:
                return self._save_m2m_func(entity)
            else:
                return entity


