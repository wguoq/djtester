from django.test import TestCase

from tester.adapter import TestCaseServiceAdapterLocal
from tester.service import TesterServicer

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
        "test_case_id": "tc1656690409",
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
            "check_point_name": "status_code != 404",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "not equals",
                "expect": "404"
            }
        },

    ],
}

case3 = {
    "test_case_type": "api",
    "tc_identity": {
        "test_case_id": "tc16566570409",
        "test_case_name": "test_case_name003"
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
            "check_point_name": "status_code == 404",
            "check_point": {
                "response_property": "status_code",
                "property_key": "",
                "operator": "equals",
                "expect": "404"
            }
        },

    ],
}

case4 = 1

class TestTT(TestCase):
    def test_service(self):
        config = TesterServicer.new_test_config()
        print(f'init_test_config ===============\n{config}')
        aaa = TesterServicer(case1).run_testcase()
        print(f'test_result ==========\n{aaa.get_test_result}')
        bbb = TesterServicer(case2).run_testcase()
        print(f'test_result ==========\n{bbb.get_test_result}')
        ccc = TesterServicer(case3).run_testcase()
        print(f'test_result ==========\n{ccc.get_test_result}')

        TestCaseServiceAdapterLocal.testcase_service().TestCaseServicer(case1).add()


        ddd = TesterServicer(case4).run_testcase()
        print(f'test_result ==========\n{ddd.get_test_result}')
