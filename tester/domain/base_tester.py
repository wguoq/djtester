import abc
import requests

from djtester.enums import TestResult
from tester.domain.tt_models import TestCaseResult, ApiTestCase, TestCheckPointResult


class BaseTester:
    def __init__(self):
        self.test_case_result: TestCaseResult = TestCaseResult()

    @staticmethod
    def _check_point_is_all_pass(check_point_result_list: list[TestCheckPointResult]):
        for result in check_point_result_list:
            if result.check_point_result == TestResult.PASS.value:
                continue
            else:
                return False
        return True

    @staticmethod
    def _check_point_is_anyone_pass(check_point_result_list: list[TestCheckPointResult]):
        for result in check_point_result_list:
            if result.check_point_result == TestResult.PASS.value:
                return True
            else:
                continue
        return False


class ApiRequestSender(object):
    def __init__(self, data: ApiTestCase):
        self.data = data if data else None
        self.payload = {}
        if self.data:
            self.payload = dict(url=data.url,
                                timeout=data.timeout,
                                allow_redirects=data.allow_redirects,
                                verify=data.verify,
                                headers=data.headers,
                                cookies=data.cookies,
                                data=data.data,
                                json=data.json_data,
                                files=data.files)

    def send_get(self):
        try:
            a = requests.get(**self.payload)
            return a
        except Exception as e:
            raise Exception(f'requests.get error:\n {e}')

    def send_post(self):
        try:
            a = requests.post(**self.payload)
            return a
        except Exception as e:
            raise Exception(f'requests.post error:\n {e}')


class ApiJsonSchemaChecker(object):
    # todo
    pass
