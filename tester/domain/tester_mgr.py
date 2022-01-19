from djtester.enums import TestCaseType
from tester.domain.api_tester import ApiTestCase, ApiTestConfig, ApiTester
from tester.domain.tester import RunTestResult


class TesterMgr:

    @staticmethod
    def run_testcase(test_case: dict, test_case_config: dict = None) -> RunTestResult:
        if test_case.get('test_case_type') == TestCaseType.API.value:
            api_case = ApiTestCase(test_case)
            api_config = ApiTestConfig(**test_case_config) if test_case_config else ApiTestConfig()
            a = ApiTester().run(api_case, api_config)
            return a
        else:
            raise Exception(f'现在只能跑api case')
