# Create your tests here.


from django.test import TestCase

from .service import *

case1 = {
    "test_case_code": "tc1622690409",
    "test_case_name": "test_case_name001",
    "test_case_type": "api",
    "version": 1,
    "tc_action": {
        "action_type": "ApiAction",
        "action_name": "get index",
        "action": {
            "method": "get",
            "protocol": "http",
            "host": "127.0.0.1",
            "port": "8000",
            "path": ""
        }
    },
    "tc_data": {
        "data_type": "ApiParams",
        "data_name": "defApiParams",
        "data": {
            "timeout": 120,
            "allow_redirects": True,
            "verify": False,
            "headers": {

            },
            "cookies": {

            },
            "data": {

            },
            "json_data": {

            },
            "files": {

            }
        }
    },
    "tc_check_list": [
        {
            "check_point_type": "ApiCheckPoint",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "rule": "",
                "operator": "eq",
                "expect": "200"
            }
        },
        {
            "check_point_type": "ApiJsonSchemaCheckPoint",
            "check_point_name": "json_schema",
            "check_point": {
                "response_property": "status_code",
                "rule": "",
                "json_schema": ""
            }
        }
    ],
}

case2 = {
    "test_case_code": "tc2622693409",
    "test_case_name": "test_case_name002",
    "test_case_type": "api",
    "version": 1,
    "tc_action": {
        "action_type": "ApiAction",
        "action_name": "get index",
        "action": {
            "method": "get",
            "protocol": "http",
            "host": "127.0.0.1",
            "port": "8000",
            "path": ""
        }
    },
    "tc_data": {
        "data_type": "ApiParams",
        "data_name": "defApiParams",
        "data": {
            "timeout": 120,
            "allow_redirects": True,
            "verify": False,
            "headers": {

            },
            "cookies": {

            },
            "data": {

            },
            "json_data": {

            },
            "files": {

            }
        }
    },
    "tc_check_list": [
        {
            "check_point_type": "ApiCheckPoint",
            "check_point_name": "status_code == 500",
            "check_point": {
                "response_property": "status_code",
                "rule": "",
                "operator": "eq",
                "expect": "500"
            }
        },
        {
            "check_point_type": "ApiJsonSchemaCheckPoint",
            "check_point_name": "json_schema",
            "check_point": {
                "response_property": "status_code",
                "rule": "",
                "json_schema": ""
            }
        }
    ],
}


class TestTestCase(TestCase):
    def test(self):
        print(f'获取一个Enums:TcEnums.case_type()=====')
        print(TestCaseEnums.test_case_type())

        print(f'api_case new =====')
        print(TestCaseService().new_api_testcase())
        #############################################################
        # print(f'Tc_Identity new =====')
        # print(TestCaseIdentityServicer.new())

        # print(f'Tc_Identity 新增 =====')
        # tc_identity1 = {"test_case_id": "tc1628342459",
        #                 "test_case_name": "tc_identity1"}
        # a = TestCaseIdentityServicer().add(tc_identity1)
        # print(a)
        # print(model_to_dict(a))

        # print(f'Tc_Identity 修改 =====')
        # tc_identity1_1 = {"id": 1,
        #                   "test_case_name": "tc_identity1_修改"}
        # a = TestCaseIdentityServicer().edit(tc_identity1_1)
        # print(model_to_dict(a))

        #############################################################
        print(f'tc_action new =====')
        print(TestCaseActionService().new())

        print(f' tc_action 新增 =====')
        tc_action1 = {"action_type": "ApiAction",
                      "action_name": "get index",
                      "action": {
                          "method": "get",
                          "protocol": "http",
                          "host": "127.0.0.1",
                          "port": "8000",
                          "path": ""}
                      }
        a = TestCaseActionService().add(tc_action1)
        print(a)

        print(f'tc_action 修改 =====')
        tc_action1_1 = {"id": 1,
                        "action_type": "ApiAction",
                        "action_name": "get index_修改",
                        }
        a = TestCaseActionService().edit(tc_action1_1)
        print(a)

        #############################################################
        print(f'tc_data new =====')
        print(TestCaseDataService().new())

        print(f' tc_data 新增 =====')
        tc_data1 = {
            "data_type": "ApiParams",
            "data_name": "defApiParams",
            "data": {
                "timeout": 120,
                "allow_redirects": True,
                "verify": False,
                "headers": {

                },
                "cookies": {

                },
                "data": {

                },
                "json_data": {

                },
                "files": {

                }
            }
        }
        a = TestCaseDataService().add(tc_data1)
        print(a)

        print(f' tc_data 修改 =====')
        tc_data1_1 = {
            "id": "1",
            "data_name": "defApiParams_修改",
        }
        a = TestCaseDataService().edit(tc_data1_1)
        print(a)

        #############################################################
        print(f'tc_check_point new =====')
        print(TestCaseCheckPointService().new())

        print(f' tc_check_point 新增 =====')
        tc_check_point1 = {
            "check_point_type": "ApiStrCheck",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "eq",
                "expect": "200"
            }
        }
        a = TestCaseCheckPointService().add(tc_check_point1)
        print(a)

        print(f' tc_check_point 修改 =====')
        tc_check_point1_1 = {
            "id": "1",
            "check_point_name": "status_code == 200_修改",
        }
        a = TestCaseCheckPointService().edit(tc_check_point1_1)
        print(a)

        #############################################################
        print(f'test_case new =====')
        print(TestCaseService().new_api_testcase())

        print(f' test_case 新增 =====')
        a = TestCaseService().add(case1)
        print(a)
        b = TestCaseService().add(case2)
        print(b)

        print(f' test_case 只修改 test_case_type =====')
        case1_1 = {"id": 1,
                   "test_case_type": "api_修改",
                   }
        a = TestCaseService().edit(case1_1)
        print(a)
        print(TestCaseService().get_by_pk(1))
        ##########################################################################
        print(f' test_case 修改 外键字段dict =====')
        case1_2 = {'id': 1, 'test_case_type': 'api_修改',
                   'tc_action': {'id': 2, 'action_type': 'ApiAction', 'action_name': 'get index-修改',
                                 'action': {'method': 'get', 'protocol': 'http', 'host': '127.0.0.1', 'port': '8000',
                                            'path': ''}},
                   'tc_data': {'id': 2, 'data_type': 'ApiParams', 'data_name': 'defApiParams=修改',
                               'data': {'timeout': 120, 'allow_redirects': True, 'verify': False, 'headers': {},
                                        'cookies': {},
                                        'data': {}, 'json_data': {}, 'files': {}}}, 'version': 1,
                   'tc_check_list': [
                       {'id': 2, 'check_point_type': 'ApiStrCheck', 'check_point_name': 'status_code == 200 修改',
                        'check_point': {'response_property': 'status_code', 'rule': '', 'operator': 'eq',
                                        'expect': '200'}}]}
        a = TestCaseService().edit(case1_2)
        print(a)
        print(TestCaseService().get_by_pk(1))
        ##############################################################################
        print(f' test_case 修改 外键字段id =====')
        case1_3 = {'id': 1, 'test_case_type': 'api_修改', 'tc_action': 1, 'tc_data': 1, 'version': 1,
                   'tc_check_list': [1, 2]}
        a = TestCaseService().edit(case1_3)
        print(a)
        print(TestCaseService().get_by_pk(1))
        #########################################################
        # print(f'用test_case_id查询')
        # a = TestCaseServicer().filter_by_case_id('tc1628342459')
        # print(a)
        #
        # print(f'用test_case_name查询')
        # a = TestCaseServicer().filter_by_case_name('tc_identity1_修改')
        # print(a)
