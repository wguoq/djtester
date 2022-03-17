import random
import time

from djtester.decorators import reg_node_func, show_class_name
from djtester.service import BaseService
from flow.domain.node_func import NodeFuncBase
from tester.domain.api_tester import ApiTestConfig
# from djtester.all_app_service import TestCaseService
from tester.domain.tester import RunTestResult
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
    def new_test_config():
        return ApiTestConfig().__dict__

    @staticmethod
    def run_testcase(test_case, test_config: dict = None) -> RunTestResult:
        """
        test_case 必须是:dict或者pk
        """
        if test_case is None:
            raise Exception(f'test_case is None')
        # 如果传入的是dict就认为是完整的testcase
        elif isinstance(test_case, dict):
            return TesterMgr.run_testcase(test_case, test_config)
        # 如果是pk
        elif isinstance(test_case, int) or isinstance(test_case, str):
            case = _get_test_case_by_pk(test_case)
            return TesterMgr.run_testcase(case, test_config)
        else:
            raise Exception(f' test_case 必须是:dict或者pk')


class NodeFuncRunApiTestCase(NodeFuncBase):

    @reg_node_func(node_type='api_tester', class_path='tester.service')
    def __init__(self):
        pass

    def node_func_param(self):
        return {'test_case_id': ''}

    def node_func_result_list(self) -> list[str]:
        pass

    def do_func(self, node_func_param: dict, flow_data):
        test_case_id = node_func_param.get('test_case_id')
        a = TesterService.run_testcase(test_case_id, flow_data)
        return self.NodeFuncResult(a.test_case_result, {'test_case_id': test_case_id})


class NodeFuncRunApiTestCaseList(NodeFuncBase):

    @reg_node_func(node_type='api_list_tester', class_path='tester.service')
    def __init__(self):
        super().__init__()

    def node_func_param(self):
        return [{'test_case_id': ''}]

    def node_func_result_list(self) -> list[str]:
        pass

    def do_func(self, node_func_param: list, flow_data):
        res = []
        for data in node_func_param:
            test_case_id = data.get('test_case_id')
            a = TesterService.run_testcase(test_case_id, flow_data)
            res.append(a.test_case_result)
        # 全部pass才行
        for r in res:
            if r != 'pass':
                return self.NodeFuncResult('fail')
            else:
                continue
        return self.NodeFuncResult('pass')
