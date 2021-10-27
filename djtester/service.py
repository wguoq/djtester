import json
from django.forms import model_to_dict
from django.core import serializers

from djtester.decorators import show_class_name


def query_set_dict_to_model_dict(query_set_dict: dict):
    pk = query_set_dict.get('pk')
    fields = query_set_dict.get('fields')
    model_dict = {}
    model_dict.update({'id': pk})
    model_dict.update(fields)
    return model_dict


class BaseServicer:
    @show_class_name('service')
    def __init__(self, db_helper):
        self.DBHelper = db_helper

    def add(self, data):
        return self.DBHelper.save_this(data)

    def edit(self, data):
        return self.DBHelper.save_this(data)

    def get_all(self, offset: int = 0, limit: int = 100) -> list[dict]:
        aaa = self.DBHelper.get_all(offset, limit)
        all_aaa = []
        for a in aaa:
            all_aaa.append(model_to_dict(a))
        return all_aaa

    def get_by_pk(self, pk: int):
        a = self.DBHelper.get_by({'pk': pk})
        return model_to_dict(a)

    def filter_by(self, kwargs: dict):
        a = self.DBHelper.filter_by(kwargs)
        s = serializers.serialize('json', a)
        dd = json.loads(s)
        dict_list = []
        for d in dd:
            dict_list.append(query_set_dict_to_model_dict(d))
        return dict_list
