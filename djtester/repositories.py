import importlib
from django.db import transaction
from django.db.models import ManyToOneRel, ManyToManyRel
from pydantic import BaseModel


def save_foreignkey(db_helper_path, db_helper_name, foreignkey_data):
    models_ = importlib.import_module(db_helper_path)
    db_helper = getattr(models_, db_helper_name)
    if foreignkey_data is None:
        return None
    # 如果传入的是dict就保存
    elif isinstance(foreignkey_data, dict):
        return db_helper().save_this(foreignkey_data)
    # 如果是int或者str就认为是pk,去查询出来
    elif isinstance(foreignkey_data, int or str):
        return db_helper().get_by(dict(pk=foreignkey_data))
    # 其他情况不处理
    else:
        return foreignkey_data


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str):
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)

    class FieldInfo(BaseModel):
        name = ''
        verbose_name = ''
        type = ''
        primary_key = False
        max_length = ''
        default = ''
        help_text = ''

    def get_field_info(self) -> list:
        fields = self.model._meta.get_fields()
        ll = []
        for f in fields:
            field_info = self.FieldInfo()
            if isinstance(f, ManyToOneRel or ManyToManyRel):
                continue
            else:
                field_info.name = f.name
                field_info.primary_key = f.primary_key
                field_info.verbose_name = f.verbose_name
                field_info.default = None if isinstance(f.default, type) else f.default
                field_info.help_text = f.help_text
                x = str(type(f))
                x = x.split('.')
                field_info.type = x[-1][:-2]
                # print(field_info)
            ll.append(field_info.__dict__)
        return ll

    def get_all(self, offset: int = 0, limit: int = 1000):
        if offset < 0 or limit < 0:
            raise Exception('offset和limit不能小于0')
        elif limit == 0:
            return self.model.objects.all()
        else:
            return self.model.objects.all()[offset: (offset + limit)]

    def get_by(self, kwargs: dict):
        return self.model.objects.get(**kwargs)

    def filter_by(self, kwargs: dict):
        return self.model.objects.filter(**kwargs)

    def _set_m2m(self, new_model):
        return new_model

    @transaction.atomic
    def save_this(self, data: dict):
        """
        如果有外键要单独保存之后把对象放进data里
        多对多的外键重写_set_m2m实现
        """
        new_model = self.model(**data)
        pk = new_model.pk
        if pk:
            # 有pk判断为修改
            # 由于update不会自动更新时间字段，还是要改成用save
            row = self.model.objects.get(pk=pk).__dict__
            row.pop('_state')
            row.update(data)
            new_model = self.model(**row)
            new_model.save()
            return self._set_m2m(new_model)
        else:
            new_model.save()
            return self._set_m2m(new_model)
