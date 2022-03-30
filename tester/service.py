import random
import time
from djtester.decorators import reg_node_func, show_class_name
from djtester.service import BaseService
from flow.domain.node_func import NodeFuncBase
from tester.domain.tester_mgr import TesterMgr
from .repositories import *


def _get_test_case_by_pk(pk):
    return TestCaseService().get_by_pk(pk)


class TestCaseService(BaseService):
    def __init__(self):
        super().__init__(TestCaseDBHelper())

    def add(self, data: dict):
        code = 'tc' + str(round(time.time()) + random.randint(0, 99))
        data.update({"code": code})
        return super().add(data)


class TcApiService(BaseService):
    def __init__(self):
        super().__init__(TcAPiDBHelper())


class TcApiDataService(BaseService):
    def __init__(self):
        super().__init__(TcApiDataDBHelper())


class TcDataService(BaseService):
    def __init__(self):
        super().__init__(TcDataDBHelper())


class TcCheckPointService(BaseService):
    def __init__(self):
        super().__init__(TcCheckPointDBHelper())


class TesterService:
    @show_class_name('service')
    def __init__(self):
        pass

    @staticmethod
    def run_testcase(test_case_pk, data_config: dict = None):
        return TesterMgr().run_case(test_case_pk, data_config)


class NodeFuncRunApiTestCase(NodeFuncBase):

    @reg_node_func(node_type='api_tester', class_path='tester.service')
    def __init__(self):
        pass

    def node_func_param(self):
        return {'test_case_id': ''}

    def node_func_result_list(self) -> list[str]:
        return ['pass', 'fail']

    def do_func(self, node_func_param: dict, flow_data: dict):
        test_case_id = node_func_param.get('test_case_id')
        a = TesterService.run_testcase(test_case_id, flow_data)
        return self.NodeFuncResult(a.test_case_result)
