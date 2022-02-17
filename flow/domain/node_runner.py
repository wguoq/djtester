import importlib
import operator
from djtester.tools_man import get_node_func_list
from flow.domain.enums import NodeStatus
from flow.models import Node_Instance, Node_Status_Rule
from flow.repositories import NodeStatusRuleDBHelper


def get_node_func_class(node_func_name: str):
    node_func_list = get_node_func_list()
    node_func = node_func_list.get(node_func_name)
    module = importlib.import_module(node_func.get('class_path'))
    return getattr(module, node_func.get('class_name'))


def check_node_status(result: str, node_instance: Node_Instance):
    # 状态规则不应该重复也不应该同时满足多条，所以只匹配一次
    rule_list = NodeStatusRuleDBHelper().filter_by({'node_design': node_instance.node_design.id})
    for rule in rule_list:
        node_status = _get_node_status_by_rule(rule, result)
        if node_status:
            return node_status
        else:
            continue
    return NodeStatus.Unknown.value


def _get_node_status_by_rule(rule: Node_Status_Rule, result: str):
    status_operator = rule.status_operator
    expect_result = rule.expect_result
    if status_operator == 'eq':
        if operator.eq(str(result), str(expect_result)):
            return rule.node_status
        else:
            return None
    elif status_operator == 'ne':
        if operator.ne(str(result), str(expect_result)):
            return rule.node_status
        else:
            return None
    else:
        raise Exception(f'不认识的operator_ {status_operator}')


class NodeInstanceRunnerResult:
    def __init__(self, node_instance: Node_Instance, return_data: dict = None):
        self.node_instance = node_instance
        self.return_data: dict = return_data if return_data else {}


class NodeInstanceRunner:
    @staticmethod
    def run(node_instance: Node_Instance, flow_data: dict = None) -> NodeInstanceRunnerResult:
        if flow_data is None:
            flow_data = {}
        # 根据 node_func_name 去载入对应执行类
        node_func_name = node_instance.node_func_name
        node_func_data = node_instance.node_func_data
        func_class = get_node_func_class(node_func_name)
        if func_class:
            try:
                node_func_result = func_class().do_func(node_func_data, flow_data)
            except Exception as e:
                raise Exception(f'运行 {func_class.__name__} 时报错 {e}')
        else:
            raise Exception(f'根据 node_func_name = {node_func_name} import 对应的class,结果为 None')
        # 运行完了就更新数据
        node_instance.node_result = node_func_result.result
        node_instance.node_status = check_node_status(node_func_result.result, node_instance)
        return NodeInstanceRunnerResult(node_instance, node_func_result.return_data)
