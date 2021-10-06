import operator

from django.forms import model_to_dict
from pydantic import BaseModel

from flow.models import Flow_Instance, Node_Design, Node_Instance, Node_Status_Rule


class NodeResultAndStatus(BaseModel):
    node_status: str = None
    node_result: str = None


class NodeMgr:
    def run_node_instance(self, node_instance: Node_Instance, flow_data: dict):
        if self._check_node_start_rule():
            run_node_result = NodeRunner().run(node_instance, flow_data)
            # 保存新的Node_Instance
            # 返回 node_status 和 node_result
            return NodeResultAndStatus(node_status=run_node_result.node_instance.node_status,
                                       node_result=run_node_result.node_instance.node_result)
        else:
            return False

    def _check_node_start_rule(self):
        # todo
        return True


class FuncResultObj(BaseModel):
    result: str = ''
    new_flow_data: dict = {}


class RunNodeResult(object):
    def __init__(self, node_instance: Node_Instance, flow_data: dict):
        self.node_instance = node_instance
        self.flow_data = flow_data


# todo
def get_func_by_node_type(node_type: str):
    if node_type == 'test123':
        return mock_func
    else:
        raise Exception(f'找不到对应 node_type={node_type} 的方法')


# todo
def mock_func(node_data, flow_data, func_result_obj):
    return FuncResultObj(result='pass', new_flow_data={'a': '123'})


class NodeRunner:
    def run(self, node_instance: Node_Instance, flow_data: dict) -> RunNodeResult:
        node_data = node_instance.node_data
        # 根据node_type去载入对应方法
        func = get_func_by_node_type(node_instance.node_design.node_type)
        # 运行方法
        run_node_result: FuncResultObj = func(node_data, flow_data, FuncResultObj())

        if run_node_result:
            # 写入node_result
            node_instance.node_result = run_node_result.result
            rule_list = node_instance.node_design.node_status_rule_set.all()
            # 写入node_status
            node_instance.node_status = self._check_node_status_by_rules(rule_list, run_node_result.result)
            # 写入new_flow_data
            new_flow_data = run_node_result.new_flow_data
        else:
            # 如果没有返回值就全部默认
            node_instance.node_result = None
            node_instance.node_status = 'unknown'
            new_flow_data = {}
        return RunNodeResult(node_instance, new_flow_data)

    def _check_node_status_by_rules(self, rule_query_list, func_result):
        for rule_query in rule_query_list:
            node_status = self._check_node_status(rule_query, func_result)
            if node_status:
                return node_status
            else:
                continue
        return 'unknown'

    @staticmethod
    def _check_node_status(rule: Node_Status_Rule, func_result):
        node_status = rule.node_status
        operator_ = rule.operator
        return_result = rule.return_result
        if operator_ == 'eq':
            if operator.eq(str(func_result), str(return_result)):
                return node_status
            else:
                return False
        elif operator_ == 'ne':
            if operator.ne(str(func_result), str(return_result)):
                return node_status
            else:
                return False
        else:
            raise Exception(f'不认识的operator_ {operator_}')
