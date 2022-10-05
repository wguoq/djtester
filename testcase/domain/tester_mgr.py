from djtester.decorators import reg_node_func
from flow.domain.node_func import NodeFuncBase
from testcase.domain.core.api_tester import *
from testcase.repositories import *


class TesterMgr:

    @staticmethod
    def run_api_case(test_case_pk, data_config: dict = None) -> dict:
        if ApiTestCaseRepository().count_by({'pk': test_case_pk}) == 0:
            raise Exception(f' 没有查询到test_case pk = {test_case_pk} ')
        test_case: ApiTestCase = ApiTestCaseRepository().filter_by_pk(test_case_pk)[0]
        r = ApiTester().run(test_case_pk, data_config)
        return dict(id=test_case.pk,
                    name=test_case.name,
                    result=r.get('result'),
                    log=r.get('log'))


class RunTestCase(NodeFuncBase):
    @reg_node_func(node_type='api_tester', class_path='testcase.domain.tester_mgr')
    def __init__(self):
        pass

    def node_func_param(self):
        return {'test_case_pk': ''}

    def node_func_result_list(self) -> list[str]:
        return ['pass', 'fail']

    def do_func(self, node_func_param: dict, flow_data: dict):
        test_case_pk = node_func_param.get('test_case_pk')
        a = TesterMgr.run_api_case(test_case_pk, flow_data)
        return self.NodeFuncResult(a.get('result'))
