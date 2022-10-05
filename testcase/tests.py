from django.forms import model_to_dict
from django.test import TestCase
from testcase.repositories import *
from testcase.domain.tester_mgr import TesterMgr

test_api = {
    "name": "api1",
    "description": "12345",
    "type": "RESTful",
    "protocol": "http",
    "method": "get",
    "path": "/",
    "param_script": {}
}
api_testcase = {
    'name': 'case1',
    'description': '12345',
    'FK_TestApi_pk': 1,
}
api_test_data = {
    'FK_ApiTestCase_pk': 1,
    'name': 'data1',
    'description': '12345',
    'host': '127.0.0.1',
    'port': 8000,
    'timeout': 3000,
    'headers': {},
    'cookies': {},
    'data': {}
}
api_test_checkpoint = {
    'FK_ApiTestData_pk': 1,
    'name': 'checkpoint1',
    'description': '12345',
    'target': 'status_code',
    'rule': '',
    'operator': 'eq',
    'expect': '200'
}


class TestTester(TestCase):
    def test(self):
        a = TestApiRepository().save_this(test_api)
        print(model_to_dict(a))

        b = ApiTestCaseRepository().save_this(api_testcase)
        print(model_to_dict(b))

        c = ApiTestDataRepository().save_this(api_test_data)
        print(model_to_dict(c))

        d = ApiTestCheckPointRepository().save_this(api_test_checkpoint)
        print(model_to_dict(d))

        e = TesterMgr.run_api_case(1)
        print(e)
