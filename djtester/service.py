import json
from django.core import serializers
from djtester.decorators import show_class_name
from djtester.repositories import BaseDBHelper


class BaseService:
    @show_class_name('service')
    def __init__(self, db_helper: BaseDBHelper):
        self.DBHelper = db_helper

    def get_field_info(self):
        return self.DBHelper.get_field_info()

    def add(self, data: dict):
        a = self.DBHelper.save_this(data).__dict__
        a.pop('_state')
        return a

    def edit(self, data: dict):
        a = self.DBHelper.save_this(data).__dict__
        a.pop('_state')
        return a

    def get_by_pk(self, pk: int) -> dict:
        a = self.DBHelper.get_by({'pk': pk}).__dict__
        a.pop('_state')
        return a

    def count_by(self, kwargs: dict):
        return self.DBHelper.count_by(kwargs)

    def filter_by(self, kwargs: dict, offset: int = 0, limit: int = 1000) -> list:
        res = self.DBHelper.filter_by(kwargs=kwargs, offset=offset, limit=limit)
        ss = serializers.serialize('json', res)
        res_dicts = json.loads(ss)
        ll = []
        for d in res_dicts:
            ll.append(self._query_set_dict_to_model_dict(d))
        return ll

    def _query_set_dict_to_model_dict(self, query_set_dict: dict):
        # 主键被显示成pk，需要把model字段的名字替换进来
        pk = query_set_dict.get('pk')
        fields = query_set_dict.get('fields')
        field_info = self.get_field_info()
        model_dict = {}
        for f in field_info:
            if f.get('primary_key'):
                model_dict.update({f.get('name'): pk})
                break
            else:
                model_dict.update({'id': pk})
        model_dict.update(fields)
        return model_dict
