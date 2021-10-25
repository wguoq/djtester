from djtester.decorators import reg_node_func
from djtester.enums import TestCaseType
from flow.domain.node_func import NodeFuncBase
from tester.domain.api_tester import ApiTester
from tester.domain.tt_models import ApiCaseConfig, ApiTestCase
from djtester.all_app_service import TestCase


def _get_test_case_by_pk(pk):
    return TestCase.service().get_by_pk(pk)


class TesterServicer:
    @staticmethod
    def new_test_config():
        return ApiCaseConfig().__dict__

    def run_testcase(self, test_case, test_config: dict = None) -> ApiTester:
        if test_case is None:
            raise Exception(f'test_case is None')
        # 如果传入的是dict就认为是完整的testcase
        elif isinstance(test_case, dict):
            a = self._run_testcase_by_dict(test_case, test_config)
            return a
        # 如果是pk
        elif isinstance(test_case, int) or isinstance(test_case, str):
            case = _get_test_case_by_pk(test_case)
            a = self._run_testcase_by_dict(case, test_config)
            return a
        else:
            raise Exception(f' test_case 必须是:dict或者pk')


    @staticmethod
    def _run_testcase_by_dict(test_case, test_case_config):
        testcase = ApiTestCase(test_case)
        if test_case_config:
            testcase_config = ApiCaseConfig(test_case_config)
        else:
            testcase_config = ApiCaseConfig()
        if testcase.test_case_type == TestCaseType.API.value:
            a = ApiTester().run(testcase, testcase_config)
            return a
        else:
            raise Exception(f'现在只能跑api case')


class NodeFuncRunApiTestCase(NodeFuncBase):
    @reg_node_func(node_type='tester', class_path='tester.service')
    def __init__(self):
        super().__init__()

    def do_func(self, node_data, flow_data):
        pass

    def node_data_model(self):
        pass


class NodeFuncTester2(NodeFuncBase):
    @reg_node_func(node_type='tester2', class_path='tester.service')
    def __init__(self):
        super().__init__()

    def do_func(self, node_data, flow_data):
        pass

    def node_data_model(self):
        pass
