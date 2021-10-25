import json
import random
import time

from django.core import serializers
from django.db import models
from django.forms import model_to_dict
from djtester.enums import TestCaseType
from testcase.domain.models.tc_api_model import *
from testcase.domain.test_case_mgr import *
from testcase.repositories import *


class TestCaseEnums:
    """
    获取testcase每个enums类里面所有的值
    """

    @staticmethod
    def test_case_type() -> list[str]:
        return list(e.value for e in TestCaseType)


class BaseTcServicer:
    def __init__(self, db_helper: models):
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
            dict_list.append(_query_set_dict_to_model_dict(d))
        return dict_list


class TestCaseIdentityServicer(BaseTcServicer):
    def __init__(self):
        self.DBHelper = TcIdentityDBHelper()
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        # 默认test_case_id是时间戳+随机数
        test_case_id = 'tc' + str(round(time.time()) + random.randint(0, 99))
        return model_to_dict(Identity(test_case_id=test_case_id))

    def add(self, data):
        if self.DBHelper.has_case_id(data) or self.DBHelper.has_case_name(data):
            raise Exception(f'test_case_id 或 test_case_name 已经存在')
        else:
            return super().add(data)


class TestCaseActionServicer(BaseTcServicer):
    def __init__(self):
        self.DBHelper = TcActionDBHelper()
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        return model_to_dict(Action())


class TestCaseDataServicer(BaseTcServicer):
    def __init__(self):
        self.DBHelper = TcDataDBHelper()
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        return model_to_dict(TestData())


class TestCaseCheckPointServicer(BaseTcServicer):
    def __init__(self):
        self.DBHelper = TcCheckPointDBHelper()
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        return model_to_dict(Check_Point())


class TestCaseServicer(BaseTcServicer):
    def __init__(self):
        self.DBHelper = TestCaseDBHelper()
        super().__init__(self.DBHelper)

    @staticmethod
    def new_api_testcase():
        test_case_id = 'tc' + str(round(time.time()) + random.randint(0, 99))
        test_case_type = TestCaseType.API.value
        tc_identity = model_to_dict(Identity(test_case_id=test_case_id))
        tc_action = model_to_dict(Action(action_type=ApiAction.__name__,
                                         action_name="",
                                         action=ApiAction().__dict__))
        tc_data = model_to_dict(TestData(data_type=ApiParams.__name__,
                                         data_name="",
                                         data=ApiParams().__dict__))
        tc_check_list = [model_to_dict(Check_Point(check_point_type=ApiCheckPoint.__name__,
                                                   check_point_name="",
                                                   check_point=ApiCheckPoint().__dict__)),
                         model_to_dict(Check_Point(check_point_type=ApiJsonSchemaCheckPoint.__name__,
                                                   check_point_name="",
                                                   check_point=ApiJsonSchemaCheckPoint().__dict__))]

        a = dict(test_case_id=test_case_id,
                 test_case_type=test_case_type,
                 tc_identity=tc_identity,
                 tc_action=tc_action,
                 tc_data=tc_data,
                 tc_check_list=tc_check_list)
        # setattr(Test_Case(), **a)
        return a

    def add(self, data):
        case_saved = self.DBHelper.save_this(data)
        case_dict = model_to_dict(case_saved)
        case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
        return case_dict

    def edit(self, data):
        case_saved = self.DBHelper.save_this(data)
        case_dict = model_to_dict(case_saved)
        case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
        return case_dict

    def get_all(self, offset=0, limit=1000):
        get_all = TestCaseDBHelper().get_all(offset=offset, limit=limit)
        all_case = []
        for a in get_all:
            case_dict = model_to_dict(a)
            all_case.append(_get_full_case(case_dict))
        return all_case

    def get_by_pk(self, pk) -> dict:
        case = TestCaseDBHelper().get_by({'pk': pk})
        case_dict = model_to_dict(case)
        return _get_full_case(case_dict)

    def filter_by(self, kwargs: dict):
        a = TestCaseDBHelper().filter_by(kwargs)
        return _query_set_to_case_dict(a)

    @staticmethod
    def filter_by_case_id(test_case_id: str):
        a = TestCaseDBHelper.filter_by_case_id(test_case_id)
        return _query_set_to_case_dict(a)

    @staticmethod
    def filter_by_case_name(test_case_name: str):
        a = TestCaseDBHelper.filter_by_case_name(test_case_name)
        return _query_set_to_case_dict(a)


def _get_check_point_pk(tc_check_list: list):
    if tc_check_list is None:
        return []
    else:
        ll = []
        for c in tc_check_list:
            ll.append(c.pk)
        return ll


def _get_full_case(case_dict) -> dict:
    """
    把testcase里面的外键关联的数据一个个都查出来,组装成一个dict
    """
    tc_identity = case_dict.get('tc_identity')
    tc_action = case_dict.get('tc_action')
    tc_data = case_dict.get('tc_data')
    tc_check_list = case_dict.get('tc_check_list')
    if tc_identity is None or len(str(tc_identity)) == 0:
        case_dict['tc_identity'] = {}
    else:
        case_dict['tc_identity'] = TestCaseIdentityServicer().get_by_pk(case_dict.get('tc_identity'))

    if tc_action is None or len(str(tc_action)) == 0:
        case_dict['tc_action'] = {}
    else:
        case_dict['tc_action'] = TestCaseActionServicer().get_by_pk(case_dict.get('tc_action'))

    if tc_data is None or len(str(tc_data)) == 0:
        case_dict['tc_data'] = {}
    else:
        case_dict['tc_data'] = TestCaseDataServicer().get_by_pk(case_dict.get('tc_data'))

    if tc_check_list is None or len(tc_check_list) == 0:
        case_dict['tc_check_list'] = []
    else:
        check_list = []
        for check in tc_check_list:
            if isinstance(check, Check_Point):
                check_list.append(model_to_dict(check))
            elif isinstance(check, int):
                c = TestCaseCheckPointServicer().get_by_pk(check)
                check_list.append(c)
        case_dict['tc_check_list'] = check_list
    return case_dict


def _query_set_dict_to_model_dict(query_set_dict: dict):
    pk = query_set_dict.get('pk')
    fields = query_set_dict.get('fields')
    model_dict = {}
    model_dict.update({'id': pk})
    model_dict.update(fields)
    return model_dict


def _query_set_to_case_dict(query_set):
    # 用serializers.serialize把QuerySet序列化成json,
    s = serializers.serialize('json', query_set)
    dd = json.loads(s)
    case_dict_list = []
    for d in dd:
        model_dict = _query_set_dict_to_model_dict(d)
        case_dict = _get_full_case(model_dict)
        case_dict_list.append(case_dict)
    return case_dict_list
