import json
import random
import time
from django.core import serializers
from django.forms import model_to_dict
from djtester.decorators import show_class_name
from djtester.enums import TestCaseType
from djtester.service import BaseService
from testcase.domain.models.tc_api_model import *
from testcase.repositories import *


class TestCaseEnums:
    """
    获取testcase每个enums类里面所有的值
    """

    @staticmethod
    def test_case_type() -> list[str]:
        return list(e.value for e in TestCaseType)


class TestCaseActionService(BaseService):
    def __init__(self):
        super().__init__(TcActionDBHelper())

    @staticmethod
    def new():
        return model_to_dict(Action())


class TestCaseDataService(BaseService):
    def __init__(self):
        super().__init__(TcDataDBHelper())

    @staticmethod
    def new():
        return model_to_dict(TestData())


class TestCaseCheckPointService(BaseService):
    def __init__(self):
        super().__init__(TcCheckPointDBHelper())

    @staticmethod
    def new():
        return model_to_dict(Check_Point())


class TestCaseService(BaseService):
    @show_class_name('service')
    def __init__(self):
        super().__init__(TestCaseDBHelper())

    @staticmethod
    def new_api_testcase():
        test_case_code = 'tc' + str(round(time.time()) + random.randint(0, 99))
        test_case_name = ''
        test_case_type = TestCaseType.API.value
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
        version = 1
        a = dict(test_case_code=test_case_code,
                 test_case_name=test_case_name,
                 test_case_type=test_case_type,
                 tc_action=tc_action,
                 tc_data=tc_data,
                 tc_check_list=tc_check_list,
                 version=version
                 )
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

    def get_by_pk(self, pk) -> dict:
        case = TestCaseDBHelper().get_by({'pk': pk})
        case_dict = model_to_dict(case)
        return _get_full_case(case_dict)


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
    tc_action = case_dict.get('tc_action')
    tc_data = case_dict.get('tc_data')
    tc_check_list = case_dict.get('tc_check_list')
    if tc_action is None or len(str(tc_action)) == 0:
        case_dict['tc_action'] = {}
    else:
        case_dict['tc_action'] = TestCaseActionService().get_by_pk(case_dict.get('tc_action'))

    if tc_data is None or len(str(tc_data)) == 0:
        case_dict['tc_data'] = {}
    else:
        case_dict['tc_data'] = TestCaseDataService().get_by_pk(case_dict.get('tc_data'))

    if tc_check_list is None or len(tc_check_list) == 0:
        case_dict['tc_check_list'] = []
    else:
        check_list = []
        for check in tc_check_list:
            if isinstance(check, Check_Point):
                check_list.append(model_to_dict(check))
            elif isinstance(check, int):
                c = TestCaseCheckPointService().get_by_pk(check)
                check_list.append(c)
        case_dict['tc_check_list'] = check_list
    return case_dict

