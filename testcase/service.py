import random
import time

from django.core import serializers
from django.forms import model_to_dict

from testcase.domain.models.tc_api_model import *
from testcase.domain.models.tc_mobile_model import *
from testcase.domain.tc_repositories import *
from testcase.domain.models.tc_ui_model import *
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


class TestCaseIdentityService:
    @staticmethod
    def init_tc_identity():
        return Tc_Identity().to_dict

    @staticmethod
    def save_identity(identity_list: list[dict]) -> list[dict]:
        aaa = []
        for identity in identity_list:
            a = TestCaseIdentityDBHelper(Tc_Identity(**identity)).save_this_one()
            aaa.append(model_to_dict(a))
        return aaa

    @staticmethod
    def get_all(offset: int = 0, limit: int = 1000) -> list[dict]:
        aaa = TestCaseIdentityDBHelper.get_all(offset, limit)
        all_aaa = []
        for a in aaa:
            all_aaa.append(model_to_dict(a))
        return all_aaa

    @staticmethod
    def get_by_pk(pk) -> dict:
        a = TestCaseIdentityDBHelper.get_by({'pk': pk})
        return model_to_dict(a)

    @staticmethod
    def filter_by_kwargs(kwargs: dict) -> dict:
        a = TestCaseIdentityDBHelper.filter_by(kwargs)
        # filter返回的是queryset需要序列化成json
        ss = serializers.serialize('json', a)
        return ss


class TestCaseActionService:
    @staticmethod
    def init_tc_action():
        return Tc_Action().to_dict

    @staticmethod
    def save_action(action_list: list[dict]):
        aaa = []
        for action in action_list:
            a = TestCaseActionDBHelper(Tc_Action(**action)).save_this_one()
            aaa.append(model_to_dict(a))
        return aaa

    @staticmethod
    def get_by_pk(pk):
        a = TestCaseActionDBHelper.get_by(dict(pk=pk))
        return model_to_dict(a)

    @staticmethod
    def get_all(offset: int = 0, limit: int = 1000):
        aaa = TestCaseActionDBHelper.get_all(offset, limit)
        all_act = []
        for a in aaa:
            all_act.append(model_to_dict(a))
        return all_act

    @staticmethod
    def filter_by_kwargs(kwargs: dict):
        a = TestCaseActionDBHelper.filter_by(kwargs)
        ss = serializers.serialize('json', a)
        return ss


class TestCaseDataService:
    @staticmethod
    def init_tc_data():
        return Tc_Data().to_dict

    @staticmethod
    def save_data(data_list: list[dict]):
        aaa = []
        for data in data_list:
            a = TestCaseDataDBHelper(Tc_Data(**data)).save_this_one()
            aaa.append(model_to_dict(a))
        return aaa

    @staticmethod
    def get_by_pk(pk):
        a = TestCaseDataDBHelper.get_by(dict(pk=pk))
        return model_to_dict(a)

    @staticmethod
    def get_all(offset: int = 0, limit: int = 1000):
        aaa = TestCaseDataDBHelper.get_all(offset, limit)
        all_td = []
        for a in aaa:
            all_td.append(model_to_dict(a))
        return all_td

    @staticmethod
    def filter_by_kwargs(kwargs: dict):
        a = TestCaseDataDBHelper.filter_by(kwargs)
        ss = serializers.serialize('json', a)
        return ss


class TestCaseCheckPointService:
    @staticmethod
    def init_tc_checkpoint():
        return Tc_Check_Point().to_dict

    @staticmethod
    def save_checkpoint(cp_list: list[dict]):
        aaa = []
        for cp in cp_list:
            a = TestCaseCheckPointDBHelper(Tc_Check_Point(**cp)).save_this_one()
            aaa.append(model_to_dict(a))
        return aaa

    @staticmethod
    def get_by_pk(pk):
        a = TestCaseCheckPointDBHelper.get_by(dict(pk=pk))
        return model_to_dict(a)

    @staticmethod
    def get_all(offset: int = 0, limit: int = 100):
        aaa = TestCaseCheckPointDBHelper.get_all(offset, limit)
        all_cp = []
        for a in aaa:
            all_cp.append(model_to_dict(a))
        return all_cp

    @staticmethod
    def filter_by_kwargs(kwargs: dict):
        a = TestCaseCheckPointDBHelper.filter_by(kwargs)
        ss = serializers.serialize('json', a)
        return ss


def _get_check_point_pk(tc_check_list: list):
    if tc_check_list is None:
        return []
    else:
        ll = []
        for c in tc_check_list:
            ll.append(c.pk)
        return ll


def _get_full_case(case_dict) -> dict:
    tc_identity = case_dict.get('tc_identity')
    tc_action = case_dict.get('tc_action')
    tc_data = case_dict.get('tc_data')
    tc_check_list = case_dict.get('tc_check_list')
    if tc_identity is None or len(str(tc_identity)) == 0:
        case_dict['tc_identity'] = {}
    else:
        case_dict['tc_identity'] = TestCaseIdentityService.get_by_pk(case_dict.get('tc_identity'))

    if tc_action is None or len(str(tc_action)) == 0:
        case_dict['tc_action'] = {}
    else:
        case_dict['tc_action'] = TestCaseActionService.get_by_pk(case_dict.get('tc_action'))

    if tc_data is None or len(str(tc_data)) == 0:
        case_dict['tc_data'] = {}
    else:
        case_dict['tc_data'] = TestCaseDataService.get_by_pk(case_dict.get('tc_data'))

    if tc_check_list is None or len(tc_check_list) == 0:
        case_dict['tc_check_list'] = []
    else:
        check_list = []
        for check in tc_check_list:
            if check:
                check_list.append(model_to_dict(check))
        case_dict['tc_check_list'] = check_list
    return case_dict


class TestCaseService:
    @staticmethod
    def init_testcase(case_type: str) -> dict:
        """
        根据case_type返回TcTestCase的dict
        """
        if case_type == CaseType.API.value:
            test_case_id = 'tc' + str(round(time.time()) + random.randint(0, 99))
            a = TcTestCase(test_case_type=case_type,
                           tc_identity=Tc_Identity(test_case_id=test_case_id).to_dict(),
                           tc_action=Tc_Action(action_type="ApiAction",
                                               action_name="", action=ApiAction().to_dict).to_dict(),
                           tc_data=Tc_Data(data_type="ApiParams",
                                           data_name="", data=ApiParams().to_dict).to_dict(),
                           tc_check_list=[Tc_Check_Point(check_point_type="ApiStrCheck",
                                                         check_point_name="",
                                                         check_point=ApiStrCheck().to_dict).to_dict(),
                                          Tc_Check_Point(check_point_type="ApiJsonSchemaCheck",
                                                         check_point_name="",
                                                         check_point=ApiJsonSchemaCheck().to_dict).to_dict()])

            return a.to_dict
        elif case_type == CaseType.UI.value:
            # todo
            return {}

        elif case_type == CaseType.MOBILE.value:
            # todo
            return {}

        else:
            raise Exception(f'没有对应 {case_type} 类型的用例模板,现在支持的类型是 {TestCaseEnums.case_type()}')

    @staticmethod
    @transaction.atomic
    def save_testcase(test_case_list: list[dict]):
        aaa = []
        for test_case in test_case_list:
            case_saved = TestCaseSDBHelper(TcTestCase(**test_case)).save_this_one()
            case_dict = model_to_dict(case_saved)
            case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
            aaa.append(case_dict)
        return aaa

    @staticmethod
    def get_all(offset=0, limit=1000):
        get_all = TestCaseSDBHelper.get_all(offset=offset, limit=limit)
        all_case = []
        for a in get_all:
            case_dict = model_to_dict(a)
            # case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
            all_case.append(_get_full_case(case_dict))
        return all_case

    @staticmethod
    def get_by_pk(pk) -> dict:
        # 要把testcase里面的外键都查出来,返回一个完整的dict
        case = TestCaseSDBHelper.get_by_pk(pk)
        case_dict = model_to_dict(case)
        # case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
        return _get_full_case(case_dict)

    @staticmethod
    def filter_by_kwargs(kwargs: dict):
        a = TestCaseSDBHelper.filter_by(kwargs)
        # filter返回的是QuerySet，用serializers.serialize序列化成json,结果格式和get方法取出来的不一样
        ss = serializers.serialize('json', a)
        return ss

    @staticmethod
    def filter_by_case_id(test_case_id_list: list) -> list:
        case_list = []
        for test_case_id in test_case_id_list:
            a = TestCaseSDBHelper.filter_by_case_id(test_case_id)
            ss = serializers.serialize('json', a)
            case_list.append(ss)
        return case_list

    @staticmethod
    def filter_by_case_name(test_case_name_list: list):
        case_list = []
        for test_case_name in test_case_name_list:
            a = TestCaseSDBHelper.filter_by_case_name(test_case_name)
            ss = serializers.serialize('json', a)
            case_list.append(ss)
        return case_list
