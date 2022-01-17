from django.test import TestCase
from tester.service import TesterServicer

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
        }
    ],
}


class TestTester(TestCase):
    def test(self):
        # print(f'===== new_test_config ======')
        # config = TesterServicer.new_test_config()
        # print(config)
        print(f'===== run_testcase case1 ======')
        aaa = TesterServicer().run_testcase(case1)
        print(aaa.__dict__)
        print(aaa.test_case_result)


