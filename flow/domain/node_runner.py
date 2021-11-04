import importlib
import operator
from djtester.tools_man import get_node_func_list
from flow.domain.enums import NodeStatus
from flow.domain.node_func import NodeFuncBase
from flow.models import Node_Instance, Node_Status_Rule


def get_node_func_class(node_func_name: str):
    node_func_list = get_node_func_list()
    node_func = node_func_list.get(node_func_name)
    module = importlib.import_module(node_func.get('class_path'))
    return getattr(module, node_func.get('class_name'))


class NodeInstanceRunner:
    def __init__(self):
        self.node_instance = None
        self._flow_data = {}
        self.return_data: dict = {}

    def run(self, node_instance: Node_Instance, flow_data: dict = None):
        if flow_data is None:
            flow_data = {}
        self.node_instance = node_instance
        self._flow_data = flow_data
        func_result: NodeFuncBase = self._run()
        # 运行完了就更新数据
        self._update_result_and_status(func_result.result)
        self.return_data = func_result.return_data
        return self

    def re_check_status(self, new_result, node_instance):
        self.node_instance = node_instance
        self.node_instance.node_result = new_result
        self.node_instance.node_status = self._check_status(new_result)
        return self.node_instance

    def _run(self):
        # 根据 node_func_name 去载入对应执行类
        node_func_name = self.node_instance.node_func_name
        node_func_data = self.node_instance.node_func_data
        func_class = get_node_func_class(node_func_name)
        if func_class:
            try:
                return func_class().do_func(node_func_data, self._flow_data)
            except Exception as e:
                raise Exception(f'运行 {func_class.__name__} 时报错 {e}')
        else:
            raise Exception(f'根据 node_func_name = {node_func_name} import 对应的class,结果为 None')

    def _update_result_and_status(self, result):
        # 节点结果就等于返回的结果
        self.node_instance.node_result = result
        self.node_instance.node_status = self._check_status(result)

    def _check_status(self, result):
        # 查询出所有 rule_list 可以是多条,约定为or关系,先到先得
        rule_list = self.node_instance.node_design.node_status_rule_set.all()
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
