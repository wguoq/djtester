import json
import random
import time

from django.core import serializers
from django.db import models
from django.forms import model_to_dict

from testcase.domain.models.tc_api_model import *
from testcase.domain.tc_model import TcTestCase
from testcase.domain.tc_repositories import *
from testcase.domain.tc_enums import *
from testcase.repositories import *


class TestCaseEnums:
    """
    获取testcase每个enums类里面所有的值
    应该换成yml文件更方便
    """

    @staticmethod
    def case_type() -> list[str]:
        return list(e.value for e in CaseType)

    @staticmethod
    def check_operator() -> list[str]:
        return list(e.value for e in CheckOperator)

    # http
    @staticmethod
    def http_method() -> list[str]:
        return list(e.value for e in HttpMethod)

    @staticmethod
    def http_protocol() -> list[str]:
        return list(e.value for e in HttpProtocol)

    @staticmethod
    def response_property() -> list[str]:
        return list(e.value for e in ResponseProperty)

    # ui
    @staticmethod
    def ui_method() -> list[str]:
        return list(e.vevalue for e in UiMethod)

    # mobil
    @staticmethod
    def mobil_method() -> list[str]:
        return list(e.value for e in MobileMethod)


class BaseTcServicer:
    def __init__(self, db_helper: models):
        self.DBHelper = db_helper

    def add(self):
        return self.DBHelper.save_this()

    def edit(self):
        return self.DBHelper.save_this()

    def get_all(self, offset: int = 0, limit: int = 1000) -> list[dict]:
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
    def __init__(self, data: dict = None):
        self.data = data
        self.DBHelper = TcIdentityDBHelper(self.data)
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        # 默认test_case_id是时间戳随机数
        test_case_id = 'tc' + str(round(time.time()) + random.randint(0, 99))
        return model_to_dict(Tc_Identity(test_case_id=test_case_id))

    def add(self):
        if self.DBHelper.has_case_id() or self.DBHelper.has_case_name():
            raise Exception(f'test_case_id 或 test_case_name 已经存在')
        else:
            return self.DBHelper.save_this()


class TestCaseActionServicer(BaseTcServicer):
    def __init__(self, data: dict = None):
        self.data = data
        self.DBHelper = TcActionDBHelper(self.data)
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        return model_to_dict(Tc_Action())


class TestCaseDataServicer(BaseTcServicer):
    def __init__(self, data: dict = None):
        self.data = data
        self.DBHelper = TcDataDBHelper(self.data)
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        return model_to_dict(Tc_Data())


class TestCaseCheckPointServicer(BaseTcServicer):
    def __init__(self, data: dict = None):
        self.data = data
        self.DBHelper = TcCheckPointDBHelper(self.data)
        super().__init__(self.DBHelper)

    @staticmethod
    def new():
        return model_to_dict(Tc_Check_Point())


class TestCaseServicer(BaseTcServicer):
    def __init__(self, data: dict = None):
        self.data = data
        self.DBHelper = TcTestCaseDBHelper(self.data)
        super().__init__(self.DBHelper)

    @staticmethod
    def new_api_testcase():
        test_case_id = 'tc' + str(round(time.time()) + random.randint(0, 99))
        test_case_type = CaseType.API.value
        tc_identity = model_to_dict(Tc_Identity(test_case_id=test_case_id))
        tc_action = model_to_dict(Tc_Action(action_type=ApiAction.__name__,
                                            action_name="",
                                            action=ApiAction().to_dict))
        tc_data = model_to_dict(Tc_Data(data_type=ApiParams.__name__,
                                        data_name="",
                                        data=ApiParams().to_dict))
        tc_check_list = [model_to_dict(Tc_Check_Point(check_point_type=ApiStrCheck.__name__,
                                                      check_point_name="",
                                                      check_point=ApiStrCheck().to_dict)),
                         model_to_dict(Tc_Check_Point(check_point_type=ApiJsonSchemaCheck.__name__,
                                                      check_point_name="",
                                                      check_point=ApiJsonSchemaCheck().to_dict))]

        a = dict(test_case_id=test_case_id,
                 test_case_type=test_case_type,
                 tc_identity=tc_identity,
                 tc_action=tc_action,
                 tc_data=tc_data,
                 tc_check_list=tc_check_list)
        # setattr(Test_Case(), **a)
        return a

    def add(self):
        case_saved = self.DBHelper.save_this()
        case_dict = model_to_dict(case_saved)
        case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
        return case_dict

    def edit(self):
        case_saved = self.DBHelper.save_this()
        case_dict = model_to_dict(case_saved)
        case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
        return case_dict

    def get_all(self, offset=0, limit=1000):
        get_all = TcTestCaseDBHelper().get_all(offset=offset, limit=limit)
        all_case = []
        for a in get_all:
            case_dict = model_to_dict(a)
            all_case.append(_get_full_case(case_dict))
        return all_case

    def get_by_pk(self, pk) -> dict:
        case = TcTestCaseDBHelper().get_by({'pk': pk})
        case_dict = model_to_dict(case)
        return _get_full_case(case_dict)

    def filter_by(self, kwargs: dict):
        a = TcTestCaseDBHelper().filter_by(kwargs)
        return _query_set_to_case_dict(a)

    @staticmethod
    def filter_by_case_id(test_case_id: str):
        a = TcTestCaseDBHelper.filter_by_case_id(test_case_id)
        return _query_set_to_case_dict(a)

    @staticmethod
    def filter_by_case_name(test_case_name: str):
        a = TcTestCaseDBHelper.filter_by_case_name(test_case_name)
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
            if isinstance(check, Tc_Check_Point):
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
