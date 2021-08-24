# Create your tests here.


from django.test import TestCase

from .service import *

case1 = {
    "test_case_type": "api",
    "tc_identity": {
        "test_case_id": "tc1622690409",
        "test_case_name": "test_case_name001"
    },
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
            "check_point_type": "ApiStrCheck",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "equals",
                "expect": "200"
            }
        },
        {
            "check_point_type": "ApiJsonSchemaCheck",
            "check_point_name": "json_schema",
            "check_point": {
                "json_schema": ""
            }
        }
    ],
}

case2 = {
    "test_case_type": "api",
    "tc_identity": {
        "test_case_id": "tc1628342459",
        "test_case_name": "test_case_name002"
    },
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
            "check_point_type": "ApiStrCheck",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "equals",
                "expect": "200"
            }
        },
        {
            "check_point_type": "ApiJsonSchemaCheck",
            "check_point_name": "json_schema",
            "check_point": {
                "json_schema": ""
            }
        }
    ],
}


class Test1(TestCase):

    def test(self):
        print(f'获取一个Enums:TcEnums.case_type()=====')
        print(TestCaseEnums.case_type())

        print(f'初始化一个api_case=====')
        print(TestCaseServicer().new_api_testcase())
        #############################################################
        print(f'Tc_Identity 初始化 =====')
        print(TestCaseIdentityServicer.new())

        print(f'Tc_Identity 新增 =====')
        tc_identity1 = {"test_case_id": "tc1628342459",
                        "test_case_name": "tc_identity1"}
        a = TestCaseIdentityServicer(tc_identity1).add()
        print(model_to_dict(a))

        print(f'Tc_Identity 修改 =====')
        tc_identity1_1 = {"id": 1,
                          "test_case_name": "tc_identity1_修改"}
        a = TestCaseIdentityServicer(tc_identity1_1).edit()
        print(model_to_dict(a))

        #############################################################
        print(f'tc_action 初始化 =====')
        print(TestCaseActionServicer().new())

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
        a = TestCaseActionServicer(tc_action1).add()
        print(model_to_dict(a))

        print(f'tc_action 修改 =====')
        tc_action1_1 = {"id": 1,
                        "action_type": "ApiAction",
                        "action_name": "get index_修改",
                        }
        a = TestCaseActionServicer(tc_action1_1).edit()
        print(model_to_dict(a))

        #############################################################
        print(f'tc_data 初始化 =====')
        print(TestCaseDataServicer().new())

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
        a = TestCaseDataServicer(tc_data1).add()
        print(model_to_dict(a))

        print(f' tc_data 修改 =====')
        tc_data1_1 = {
            "id": "1",
            "data_name": "defApiParams_修改",
        }
        a = TestCaseDataServicer(tc_data1_1).edit()
        print(model_to_dict(a))

        #############################################################
        print(f'tc_check_point 初始化 =====')
        print(TestCaseCheckPointServicer().new())

        print(f' tc_check_point 新增 =====')
        tc_check_point1 = {
            "check_point_type": "ApiStrCheck",
            "check_point_name": "status_code == 200",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "equals",
                "expect": "200"
            }
        }
        a = TestCaseCheckPointServicer(tc_check_point1).add()
        print(model_to_dict(a))

        print(f' tc_check_point 修改 =====')
        tc_check_point1_1 = {
            "id": "1",
            "check_point_name": "status_code == 200_修改",
        }
        a = TestCaseCheckPointServicer(tc_check_point1_1).edit()
        print(model_to_dict(a))

        #############################################################
        print(f'test_case 初始化 =====')
        print(TestCaseServicer().new_api_testcase())

        print(f' test_case 新增 =====')
        a = TestCaseServicer(case1).add()
        print(a)

        print(f' test_case 修改 =====')
        case1_1 = {"id": 1,
                   "test_case_type": "api_修改",
                   }
        a = TestCaseServicer(case1_1).edit()
        print(a)
