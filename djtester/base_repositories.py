import importlib
from django.db import transaction


class BaseDBHelper:
    def __init__(self, model_path: str, model_name: str, data: dict = None):
        self.data = data
        # 由于model.objects必须用本来的model来调用,所以import对应的model
        models = importlib.import_module(model_path)
        self.model = getattr(models, model_name)
        self.entity = self.model()
        if self.data:
            self.entity = self.model(**self.data)

        # 取出所有field.name以后用
        # self.fields_name = []
        # for f in self.entity._meta.fields:
        #     self.fields_name.append(f.name)


    @transaction.atomic
    def save_this(self):
        pk = self.entity.pk
        if pk:
            # 有pk判断为修改
            # 先把原始数据查出来
            # original = self.model.objects.get(pk=pk).__dict__
            # # 把新数据的内容写进来,只写不为None的字段
            # for field in self.fields_name:
            #     if self.data.get(field):
            #         original[field] = self.data.get(field)
            #     else:
            #         continue
            # # 根据fields_name过滤出数据
            # new = {}
            # for field in self.fields_name:
            #     new.update({field: original[field]})
            # # 生成新的实体保存
            # new_entity = self.model(**new)
            # new_entity.save()
            # return new_entity
            self.model.objects.filter(pk=pk).update(**self.data)
            return self.model.objects.get(pk=pk)
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

