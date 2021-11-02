from djtester.decorators import reg_node_func, show_class_name
from djtester.enums import TestCaseType
from flow.domain.node_func import NodeFuncBase
from tester.domain.api_tester import ApiTester
from tester.domain.api_tester import ApiTestConfig, ApiTestCase
from djtester.all_app_service import TestCaseService


def _get_test_case_by_pk(pk):
    test_case_servicer = TestCaseService.test_case_servicer()
    return test_case_servicer().get_by_pk(pk)


class TesterServicer:
    @show_class_name('service')
    def __init__(self):
        pass

    @staticmethod
    def new_test_config():
        return ApiTestConfig().__dict__

    def run_testcase(self, test_case, test_config: dict = None) -> ApiTester:
        """
        test_case 必须是:dict或者pk
        """
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
    def _run_testcase_by_dict(test_case: dict, test_case_config: dict):
        testcase = ApiTestCase(test_case)
        if test_case_config:
            testcase_config = ApiTestConfig(**test_case_config)
        else:
            testcase_config = ApiTestConfig()
        if testcase.test_case_type == TestCaseType.API.value:
            a = ApiTester().run(testcase, testcase_config)
            return a
        else:
            raise Exception(f'现在只能跑api case')


class NodeFuncRunApiTestCase(NodeFuncBase):

    @reg_node_func(node_type='api_tester', class_path='tester.service')
    def __init__(self):
        super().__init__()

    def node_func_data_model(self):
        return {'test_case_id': ''}

    def node_func_result_list(self) -> list[str]:
        pass

    def do_func(self, node_func_data: dict, flow_data):
        test_case_id = node_func_data.get('test_case_id')
        a = TesterServicer().run_testcase(test_case_id, flow_data)
        self.result = a.test_case_result
        self.return_data = {'test_case_id': test_case_id}
        return self


class NodeFuncRunApiTestCaseList(NodeFuncBase):

    @reg_node_func(node_type='api_list_tester', class_path='tester.service')
    def __init__(self):
        super().__init__()

    def node_func_data_model(self):
        return [{'test_case_id': ''}]

    def node_func_result_list(self) -> list[str]:
        pass

    def do_func(self, node_func_data: list, flow_data):
        res = []
        for data in node_func_data:
            test_case_id = data.get('test_case_id')
            a = TesterServicer().run_testcase(test_case_id, flow_data)
            res.append(a.test_case_result.case_result)
        for r in res:
            if r != 'pass':
                self.result = 'fail'
                return self
            else:
                continue
        self.result = 'pass'
        return self
