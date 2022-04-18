from djtester.decorators import reg_node_func
from flow.domain.node_func import NodeFuncBase
from tester.domain.api_tester import ApiTester
from tester.repositories import *


class TesterMgr:

    @staticmethod
    def run_case(test_case_pk, data_config: dict = None):
        if TestCaseDBHelper().count_by({'pk': test_case_pk}) == 0:
            raise Exception(f'没有查询到test_case pk = {test_case_pk}')
        test_case: TestCase = TestCaseDBHelper().get_by_pk(test_case_pk)[0]
        if test_case.tc_type == 'api':
            r = ApiTester().run(test_case_pk, data_config)
            return dict(id=test_case.pk,
                        code=test_case.code,
                        name=test_case.tc_name,
                        result=r.get('result'),
                        log=r.get('log'))
        else:
            raise Exception(f'无法识别的 tc_type {test_case.tc_type}')


class RunTestCase(NodeFuncBase):
    @reg_node_func(node_type='api_tester', class_path='tester.domain.tester_mgr')
    def __init__(self):
        pass

    def node_func_param(self):
        return {'test_case_pk': ''}

    def node_func_result_list(self) -> list[str]:
        return ['pass', 'fail']

    def do_func(self, node_func_param: dict, flow_data: dict):
        test_case_pk = node_func_param.get('test_case_pk')
        a = TesterMgr.run_case(test_case_pk, flow_data)
        return self.NodeFuncResult(a.get('result'))
