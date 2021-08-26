from tester.domain.api_tester import ApiTester
from tester.domain.tt_models import ApiCaseConfig
from tester.adapter import *


def _get_test_case_by_pk(pk):
    return TestCaseServiceAdapterLocal().get_test_case_by_pk(pk)


class TesterServicer:
    def __init__(self, test_case, test_config: dict = None):
        self.test_case = test_case
        self.test_config = test_config

    @staticmethod
    def new_test_config():
        return ApiCaseConfig().to_dict()

    def run_testcase(self):
        if self.test_case is None:
            return None
        elif isinstance(self.test_case, dict):
            return self._run_testcase_by_dict(self.test_case)
        else:
            case = _get_test_case_by_pk(self.test_case)
            return self._run_testcase_by_dict(case)

    def _run_testcase_by_dict(self, test_case):
        if test_case.get('test_case_type') == "api":
            a = ApiTester(api_test_case=test_case, api_test_config=self.test_config).run_test_case()
            return a
        else:
            print(f'test_case_type != api')
            return None






