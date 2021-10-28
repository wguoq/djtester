import importlib
import operator
from djtester.tools_man import get_node_func_list
from flow.domain.enums import NodeStatus
from flow.domain.node_func import NodeFuncBase
from flow.models import Node_Instance, Node_Status_Rule


def get_node_func(node_func_name: str):
    node_func_list = get_node_func_list()
    node_func = node_func_list.get(node_func_name)
    module = importlib.import_module(node_func.get('class_path'))
    return getattr(module, node_func.get('class_name'))


class NodeInstanceRunner:
    def __init__(self):
        self.new_node_instance = None
        self.return_data: dict = {}

    def run(self, node_instance: Node_Instance, flow_data: dict = None):
        if flow_data is None:
            flow_data = {}
        func_result: NodeFuncBase = self._run(node_instance, flow_data)
        # 运行完了就更新数据
        self.new_node_instance = self.update_result_status(func_result.result, node_instance)
        self.return_data = func_result.return_data
        return self

    @staticmethod
    def _run(node_instance: Node_Instance, flow_data):
        # 根据 node_func_name 去载入对应执行类
        node_func_name = node_instance.node_func_name
        node_func_data = node_instance.node_func_data
        func_class = get_node_func(node_func_name)
        if func_class:
            try:
                return func_class().do_func(node_func_data, flow_data)
            except Exception as e:
                raise Exception(f'运行 {func_class.__name__} 时报错 {e}')
        else:
            raise Exception(f'根据 node_func_name = {node_func_name} import 对应的class,结果为 None')

    def update_result_status(self, result: str, node_instance):
        node_instance.node_result = result
        node_instance.node_status = self._check_status(result, node_instance)
        return node_instance

    def _check_status(self, result: str, node_instance):
        rule_list = node_instance.node_design.node_status_rule_set.all()
        for rule in rule_list:
            node_status = self._get_node_status_by_rule(rule, result)
            if node_status:
                return node_status
            else:
                continue
        return NodeStatus.Unknown.value

    @staticmethod
    def _get_node_status_by_rule(rule: Node_Status_Rule, result):
        status_operator = rule.status_operator
        status_target = rule.status_target
        if status_operator == 'eq':
            if operator.eq(str(result), str(status_target)):
                return rule.node_status
            else:
                return None
        elif status_operator == 'ne':
            if operator.ne(str(result), str(status_target)):
                return rule.node_status
            else:
                return None
        else:
            raise Exception(f'不认识的operator_ {status_operator}')
