import abc
import requests

from djtester.enums import TestResult
from tester.domain.tt_models import TestCaseResult, ApiTestCase, TestCheckPointResult


class BaseTester:
    def __init__(self, test_case):
        self.test_case = test_case if test_case else None
        self.check_point_result_list: list[TestCheckPointResult] = []
        self.test_case_result = TestCaseResult()

    def run(self):
        self.do_action()
        self.verify_check_point()
        self.set_test_case_result()
        return self

    @abc.abstractmethod
    def do_action(self):
        pass

    @abc.abstractmethod
    def verify_check_point(self):
        pass

    @abc.abstractmethod
    def set_test_case_result(self):
        pass

    def check_point_is_all_pass(self):
        for result in self.check_point_result_list:
            if result.check_point_result == TestResult.PASS.value:
                continue
            else:
                return False
        return True

    def check_point_is_anyone_pass(self):
        for result in self.check_point_result_list:
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
