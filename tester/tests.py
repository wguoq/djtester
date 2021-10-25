from django.test import TestCase

from tester.domain.api_tester import ApiTester
from tester.domain.tt_models import ApiTestCase
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


class TestTester(TestCase):
    def test(self):
        # print(f'===== new_test_config ======')
        # config = TesterServicer.new_test_config()
        # print(config)
        print(f'===== run_testcase ======')
        aaa = TesterServicer().run_testcase(case1)
        print(aaa.test_case_result)
        #print(f'test_result ==========\n{aaa.test_case_result.__dict__}')

