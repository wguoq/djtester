import operator

from django.forms import model_to_dict
from pydantic import BaseModel

from flow.models import Node_Instance, Node_Status_Rule
from flow.repositories import NodeInstanceDBHelper


class NodeMgrResult(BaseModel):
    node_status: str
    node_result: str
    new_flow_data: dict


class NodeMgr:
    def run_node_instance(self, node_instance: Node_Instance, flow_data: dict):
        if self._check_node_start_rule(node_instance):
            run_node_result = NodeInstanceRunner().run(node_instance, flow_data)
            new_flow_data = run_node_result.new_flow_data
            # 保存新的Node_Instance
            new_node_instance = run_node_result.node_instance
            NodeInstanceDBHelper().save_this(model_to_dict(new_node_instance))
            # 返回 node_status 和 node_result
            return NodeMgrResult(node_status=new_node_instance.node_status,
                                 node_result=new_node_instance.node_result,
                                 new_flow_data=new_flow_data)
        else:
            return NodeMgrResult(node_status=node_instance.node_status,
                                 node_result=node_instance.node_result,
                                 new_flow_data=flow_data)

    def _check_node_start_rule(self, node_instance):
        # todo
        # 1.node 状态不为finish和stop才执行
        # 2.满足start_rule
        return True


class FuncResultDTO(BaseModel):
    result: str = ''
    new_flow_data: dict = {}


class NodeInstanceRunnerResult(object):
    def __init__(self, node_instance: Node_Instance, new_flow_data: dict):
        self.node_instance = node_instance
        self.new_flow_data = new_flow_data


# todo
def get_func_by_node_type(node_type: str):
    if node_type == 'test123':
        return mock_func
    else:
        raise Exception(f'找不到对应 node_type={node_type} 的方法')


# todo
def mock_func(node_data, flow_data, func_result_obj):
    return FuncResultDTO(result='pass', new_flow_data={'a': '123'})


class NodeInstanceRunner:
    def run(self, node_instance: Node_Instance, flow_data: dict) -> NodeInstanceRunnerResult:
        node_data = node_instance.node_data
        # 根据node_type去载入对应方法
        func = get_func_by_node_type(node_instance.node_design.node_type)
        # 运行方法
        run_node_result: FuncResultDTO = func(node_data, flow_data, FuncResultDTO())
        if isinstance(run_node_result,FuncResultDTO) is False:
            raise Exception(f'func = {func.__name__} 返回的类型不是 FuncResultDTO, 无法处理')
        if run_node_result:
            # 写入node_result
            node_instance.node_result = run_node_result.result
            # 通过外键反向查询node_status_rule
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
        return NodeInstanceRunnerResult(node_instance, new_flow_data)

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
