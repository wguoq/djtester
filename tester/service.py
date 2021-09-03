from djtester.enums import TestCaseType
from tester.domain.api_tester import ApiTester
from tester.domain.tt_models import ApiCaseConfig, ApiTestCase
from tester.adapter import *


def _get_test_case_by_pk(pk):
    return TestCaseServiceAdapterLocal().get_test_case_by_pk(pk)


class TesterServicer:
    def __init__(self, test_case, test_config: dict = None):
        self.test_case = test_case
        self.test_config = test_config

    @staticmethod
    def new_test_config():
        return ApiCaseConfig().__dict__

    def run_testcase(self) -> ApiTester:
        if self.test_case is None:
            raise Exception(f'test_case is None')
        # 如果传入的是dict就认为是完整的testcase
        elif isinstance(self.test_case, dict):
            return self._run_testcase_by_dict(self.test_case)
        # 否则就认为是pk,要自己去查出来
        else:
            case = _get_test_case_by_pk(self.test_case)
            return self._run_testcase_by_dict(case)

    def _run_testcase_by_dict(self, test_case):
        case = ApiTestCase(test_case)
        if case.test_case_type == TestCaseType.API.value:
            a = ApiTester(api_test_case=case, api_test_config=self.test_config).run()
            return a
        else:
            raise Exception(f'现在只能跑api case')







