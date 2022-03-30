import importlib
from django.db.models import ManyToOneRel, ManyToManyRel, QuerySet
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
            ll.append(field_info.__dict__)
        return ll

    # def get_by(self, kwargs: dict):
    #     return self.model.objects.get(**kwargs)

    def count_by(self, kwargs: dict) -> int:
        return self.model.objects.filter(**kwargs).count()

    def filter_by(self, kwargs: dict = None, offset: int = 0, limit: int = 1000) -> QuerySet:
        if offset <= 0:
            offset = 0
        else:
            pass
        if limit <= 0:
            limit = 1000
        else:
            pass
        if limit - offset > 1000:
            limit = offset + 1000
        else:
            pass
        if kwargs is None:
            kwargs = {}
        return self.model.objects.filter(**kwargs)[offset: limit]

    def save_this(self, data: dict):
        """
        如果有外键要单独保存之后把对象放进data里
        """
        new_model = self.model(**data)
        pk = new_model.pk
        if pk:
            # 有pk判断为修改
            # 由于update不会自动更新时间字段，还是要改成用save
            # 要先把要修改的那条数据查询出来，获取完整的模型字段
            row = self.model.objects.get(pk=pk).__dict__
            row.pop('_state')
            row.update(data)
            new_model = self.model(**row)
            new_model.save()
            return new_model
        else:
            new_model.save()
            return new_model

    def del_item(self, filters: dict):
        return self.model.objects.filter(**filters).delete()

