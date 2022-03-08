import json
from django.forms import model_to_dict
from django.core import serializers
from djtester.decorators import show_class_name
from djtester.repositories import BaseDBHelper


def query_set_dict_to_model_dict(query_set_dict: dict):
    pk = query_set_dict.get('pk')
    fields = query_set_dict.get('fields')
    model_dict = {}
    model_dict.update({'id': pk})
    model_dict.update(fields)
    return model_dict


class BaseService:
    @show_class_name('service')
    def __init__(self, db_helper: BaseDBHelper):
        self.DBHelper = db_helper

    def get_field_info(self):
        return self.DBHelper.get_field_info()

    def add(self, data: dict):
        a = self.DBHelper.save_this(data)
        x = a.__dict__
        x.pop('_state')
        return x

    def edit(self, data: dict):
        a = self.DBHelper.save_this(data)
        x = a.__dict__
        x.pop('_state')
        return x

    def get_all(self, offset: int = 0, limit: int = 100) -> list[dict]:
        # 切片和排序不能写一起
        # query_set = self.DBHelper.get_all(offset, limit).order_by(order)
        query_set = self.DBHelper.get_all(offset, limit)
        res = []
        for a in query_set:
            # res.append(model_to_dict(a))
            x = a.__dict__
            x.pop('_state')
            res.append(x)
        return res

    def get_by_pk(self, pk: int):
        a = self.DBHelper.get_by({'pk': pk})
        x = a.__dict__
        x.pop('_state')
        return x

    def filter_by(self, kwargs: dict):
        query_set = self.DBHelper.filter_by(kwargs)
        ss = serializers.serialize('json', query_set)
        query_set_dicts = json.loads(ss)
        res = []
        for d in query_set_dicts:
            res.append(query_set_dict_to_model_dict(d))
        return res
