import json
from django.core import serializers
from django.forms import model_to_dict
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
    """
    把testcase里面的外键关联的数据都查出来,组装成一个dict
    """
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
            if isinstance(check, Tc_Check_Point):
                check_list.append(model_to_dict(check))
            elif isinstance(check, int):
                c = TestCaseCheckPointService.get_by_pk(check)
                check_list.append(c)
        case_dict['tc_check_list'] = check_list
    return case_dict


def _query_set_to_case_dict(query_set):
    # 用serializers.serialize把QuerySet序列化成json,
    s = serializers.serialize('json', query_set)
    dd = json.loads(s)
    query_set_dict = []
    for d in dd:
        pk = d.get('pk')
        fields = d.get('fields')
        case_dict = {}
        case_dict.update({'id': pk})
        case_dict.update(fields)
        full_case = _get_full_case(case_dict)
        query_set_dict.append(full_case)
    return query_set_dict


class TestCaseService:
    class new:
        """
        返回一个空的api_test_case数据模板
        """
        @staticmethod
        def api_test_case():
            return TcTestCase().new_api_test_case()

    @staticmethod
    @transaction.atomic
    def save(test_case_list: list[dict]):
        """
        保存test_case_list,新增和编辑都一起
        """
        aaa = []
        for test_case in test_case_list:
            case_saved = TestCaseSDBHelper(TcTestCase(**test_case)).save_this_one()
            case_dict = model_to_dict(case_saved)
            case_dict['tc_check_list'] = _get_check_point_pk(case_dict.get('tc_check_list'))
            aaa.append(case_dict)
        return aaa

    class query:
        @staticmethod
        def get_all(offset=0, limit=1000):
            get_all = TestCaseSDBHelper.get_all(offset=offset, limit=limit)
            all_case = []
            for a in get_all:
                case_dict = model_to_dict(a)
                all_case.append(_get_full_case(case_dict))
            return all_case

        @staticmethod
        def get_by_pk(pk) -> dict:
            case = TestCaseSDBHelper.get_by_pk(pk)
            case_dict = model_to_dict(case)
            return _get_full_case(case_dict)

        @staticmethod
        def filter_by(kwargs: dict):
            a = TestCaseSDBHelper.filter_by(kwargs)
            return _query_set_to_case_dict(a)

        @staticmethod
        def filter_by_case_id(test_case_id_list: list) -> list:
            case_list = []
            for test_case_id in test_case_id_list:
                a = TestCaseSDBHelper.filter_by_case_id(test_case_id)
                case_list.append(_query_set_to_case_dict(a))
            return case_list

        @staticmethod
        def filter_by_case_name(test_case_name_list: list):
            case_list = []
            for test_case_name in test_case_name_list:
                a = TestCaseSDBHelper.filter_by_case_name(test_case_name)
                case_list.append(_query_set_to_case_dict(a))
            return case_list






