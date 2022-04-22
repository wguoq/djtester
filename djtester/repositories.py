import importlib
import json
from django.core import serializers
from django.db.models import ManyToOneRel, ManyToManyRel, QuerySet, ForeignKey
from pydantic import BaseModel


def save_foreignkey(app_name, db_helper_name, foreignkey_data):
    repo_path = app_name + '.repositories'
    models_ = importlib.import_module(repo_path)
    db_helper = getattr(models_, db_helper_name)
    if foreignkey_data is None:
        return None
    # 如果传入的是dict就保存
    elif isinstance(foreignkey_data, dict):
        return db_helper().save_this(foreignkey_data)
    # 否则认为是pk,去查询出来
    else:
        if db_helper().count_by({'pk': foreignkey_data}) == 0:
            return None
        else:
            return db_helper().get_by_pk(foreignkey_data)[0]


class BaseDBHelper:
    def __init__(self, app_name: str, model_name: str):
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        model_path = app_name + '.models'
        self.models = importlib.import_module(model_path)
        self.model_name = model_name
        self.model = getattr(self.models, model_name)

    class FieldInfo(BaseModel):
        name = ''
        verbose_name = ''
        type = ''
        primary_key = False
        max_length = ''
        default = ''
        required = True
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
                field_info.required = False if f.null else True
                field_info.help_text = f.help_text
                x = str(type(f))
                x = x.split('.')
                field_info.type = x[-1][:-2]
            ll.append(field_info.__dict__)
        return ll

    def get_table_info(self) -> list:
        aa = self.get_field_info()
        for a in aa:
            a['name'] = self.model_name + '__' + a.get('name')
        return aa

    def get_pk_name(self):
        field_info = self.get_field_info()
        for field in field_info:
            if field.get('primary_key'):
                return field.get('name')
            else:
                continue
        return None

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

    def get_by_pk(self, pk) -> QuerySet:
        return self.filter_by({'pk': pk})

    def _get_fk_inst(self, data: dict):
        """
        如果要自己处理外键就重写这个方法
        """
        # 尝试去读取外键数据，并替换进data
        for field in self.model._meta.get_fields():
            if isinstance(field, ForeignKey):
                field_name = field.name
                related_model = field.related_model
                try:
                    fk_data = related_model.objects.get(pk=data.get(field_name))
                    data.update({field_name: fk_data})
                except Exception as e:
                    # 如果没有get到数据那就赋值为None
                    data.update({field_name: None})
            else:
                continue
        return data

    def save_this(self, data: dict):
        data = self._get_fk_inst(data)
        new_model = self.model(**data)
        pk = new_model.pk
        if pk:
            # 有pk判断为修改
            # 由于update不会自动更新时间字段，还是要改成用save
            # 要先把要修改的那条数据查询出来，获取完整的模型字段
            # 有外键不能用  row = self.model.objects.get(pk=pk).__dict__
            res = self.filter_by({'pk': pk})
            ss = serializers.serialize('json', res)
            r = json.loads(ss)[0]
            fields = r.get('fields')
            fields.update(data)
            new_model = self.model(**fields)
            new_model.save()
            return new_model
        else:
            new_model.save()
            return new_model

    def del_(self, filters: dict):
        if filters is None or len(filters) == 0:
            raise Exception('del 的filters参数不能为空')
        else:
            return self.model.objects.filter(**filters).delete()
