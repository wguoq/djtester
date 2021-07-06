from tester.domain.api_tester import ApiTester
from tester.domain.tt_models import ApiCaseConfig



class TesterService:

    @staticmethod
    def init_test_config():
        return ApiCaseConfig().to_dict()

    @staticmethod
    def run_testcase(test_case: dict, test_config: dict = None):

        if test_case.get('test_case_type') == "api":
            a = ApiTester(api_test_case=test_case, api_test_config=test_config).run_test_case()
            return a
        else:
            print(f'test_case_type != api')





