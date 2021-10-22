import operator

from flow.domain.node_func import NodeFuncBase
from flow.models import Node_Instance, Node_Status_Rule


def get_class_by_node_type(node_type: str):
    # todo
    if node_type == 'test123':
        return MockFunc


class MockFunc(NodeFuncBase):

    def do_func(self, node_data, flow_data):
        self.result = 'pass'
        self.return_data = {}
        return self


class NodeInstanceRunner:
    def __init__(self):
        self.new_node_instance = None
        self.return_data: dict = {}

    def run(self, node_instance: Node_Instance, flow_data: dict = None):
        node_data: dict = node_instance.node_data
        # 运行对应的func
        func_result: NodeFuncBase = self._run(node_instance, node_data, flow_data)
        # 运行完了就更新数据
        self.new_node_instance = self._update_result_status(func_result, node_instance)
        self.return_data = func_result.return_data
        return self

    @staticmethod
    def _run(node_instance: Node_Instance, node_data, flow_data):
        # 根据node_type去载入对应执行类
        node_type = node_instance.node_design.node_type
        func_class = get_class_by_node_type(node_type)
        try:
            return func_class().do_func(node_data, flow_data)
        except Exception as e:
            raise Exception(f'根据node_type去载入对应的执行类 {func_class.__name__} ,但是运行时报错 {e}')

    def _update_result_status(self, func_result, node_instance):
        node_instance.result_target = func_result.result
        node_instance.node_status = self._check_status(func_result, node_instance)
        return node_instance

    def _check_status(self, func_result, node_instance):
        rule_list = node_instance.node_design.node_status_rule_set.all()
        for rule in rule_list:
            node_status = self._get_node_status_by_rule(rule, func_result)
            if node_status:
                return node_status
            else:
                continue
        return 'unknown'

    @staticmethod
    def _get_node_status_by_rule(rule: Node_Status_Rule, func_result):
        status_operator = rule.status_operator
        status_target = rule.status_target
        if status_operator == 'eq':
            if operator.eq(str(func_result.result), str(status_target)):
                return rule.node_status
            else:
                return None
        elif status_operator == 'ne':
            if operator.ne(str(func_result.result), str(status_target)):
                return rule.node_status
            else:
                return None
        else:
            raise Exception(f'不认识的operator_ {status_operator}')
