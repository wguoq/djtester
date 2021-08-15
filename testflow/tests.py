from django.forms import model_to_dict
from django.test import TestCase

# Create your tests here.

from django.test import TestCase

from testflow.domain.test_flow import TestFlow
from testflow.models import Test_Flow
from testflow.repositories import TestFlowSDBHelper

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

test_flow1 = {
    "id": None,
    "name": 'test_flow1',
    "result_rule": {},
    "result": None,
    "status": None,
    "data": {},
    "step_list": [
        {
            "step_id": None,
            "step_type": "test_case",
            "step_pk": 1,
            "step_name": "step01",
            "step_order": 0,
            "step_rule": {},
            "step_result": None,
            "step_status": None,
            "dynamic_data_rule": {},
        },
        {
            "step_id": None,
            "step_type": "test_case",
            "step_pk": 2,
            "step_name": "step02",
            "step_order": 1,
            "step_rule": {},
            "step_result": None,
            "step_status": None,
            "dynamic_data_rule": {},
        },
    ]
}

test_flow2 = {
    "id": None,
    "name": 'test_flow2',
    "result_rule": {},
    "result": None,
    "status": None,
    "data": {},
    "step_list": [
        {
            "step_id": None,
            "step_type": "test_flow",
            "step_pk": 1,
            "step_name": "step001",
            "step_order": 0,
            "step_rule": {},
            "step_result": None,
            "step_status": None,
            "dynamic_data_rule": {},
        },
        {
            "step_id": None,
            "step_type": "test_flow",
            "step_pk": 1,
            "step_name": "step002",
            "step_order": 1,
            "step_rule": {},
            "step_result": None,
            "step_status": None,
            "dynamic_data_rule": {},
        },
    ]
}

test_flow3 = {
    "id": None,
    "name": 'test_flow2',
    "result_rule": {},
    "result": None,
    "status": None,
    "data": {},
    "step_list": [
        {
            "step_id": None,
            "step_type": "test_flow",
            "step_pk": 3,
            "step_name": "step003",
            "step_order": 0,
            "step_rule": {},
            "step_result": None,
            "step_status": None,
            "dynamic_data_rule": {},
        },
    ]
}

class TestTF(TestCase):
    def test_tflow(self):
        from djtester.testcase_service import TestCaseService
        TestCaseService.save([case1, case2])
        f1 = TestFlowSDBHelper(Test_Flow(**test_flow1)).save_this_one()
        print(f'f1===\n {model_to_dict(f1)}')
        f2 = TestFlowSDBHelper(Test_Flow(**test_flow2)).save_this_one()
        print(f'f2===\n {model_to_dict(f2)}')
        f3 = TestFlowSDBHelper(Test_Flow(**test_flow3)).save_this_one()
        print(f'f3===\n {model_to_dict(f3)}')

        tf1 = TestFlow(model_to_dict(f1)).run()
        print(tf1.name)
        print(tf1.result)
        print(tf1.status)
        print(tf1.step_result_list)

        tf2 = TestFlow(model_to_dict(f2)).run()
        print(tf2.name)
        print(tf2.result)
        print(tf2.status)
        print(tf2.step_result_list)

        tf3 = TestFlow(model_to_dict(f3)).run()
        print(tf3.name)
        print(tf3.result)
        print(tf3.status)
        print(tf3.step_result_list)

