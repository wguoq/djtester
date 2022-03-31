from djtester.decorators import reg_node_func
from flow.domain.node_func import NodeFuncBase
from tester.domain.api_tester import ApiTester
from tester.repositories import *


class TesterMgr:

    @staticmethod
    def run_case(test_case_pk, data_config: dict = None):
        test_case: Test_Case = TestCaseDBHelper().get_by_pk(test_case_pk)[0]
        if test_case.tc_type == 'api':
            r = ApiTester().run(test_case_pk, data_config)
            return dict(id=test_case.pk,
                        code=test_case.code,
                        name=test_case.tc_name,
                        result=r.get('result'),
                        log=r.get('log'))
        else:
            raise Exception(f'无法识别的 tc_type {test_case.tc_type}')


class NodeFuncRunApiTestCase(NodeFuncBase):
    @reg_node_func(node_type='api_tester', class_path='tester.service')
    def __init__(self):
        pass

    def node_func_param(self):
        return {'pk': ''}

    def node_func_result_list(self) -> list[str]:
        return ['pass', 'fail']

    def do_func(self, node_func_param: dict, flow_data: dict):
        test_case_pk = node_func_param.get('pk')
        a = TesterMgr.run_case(test_case_pk, flow_data)
        return self.NodeFuncResult(a.get('result'))
