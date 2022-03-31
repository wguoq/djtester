from django.forms import model_to_dict
from djtester.tools_man import verify_str
from flow.domain.enums import NodeStatus, NodeStartRuleTarget, NodeStartRuleType
from flow.domain.node_runner import NodeInstanceRunner, check_node_status
from flow.models import Node_Instance, Node_Start_Rule
from flow.repositories import NodeInstanceDBHelper, NodeStartRuleDBHelper


def _check_rule(start_rule: Node_Start_Rule, flow_data: dict, node_instance: Node_Instance):
    rule_target = start_rule.rule_target
    rule_where = start_rule.rule_where
    rule_operator = start_rule.rule_operator
    rule_value = start_rule.rule_value
    if rule_target == NodeStartRuleTarget.FlowData.value:
        # 约定 rule_where 填的是 flow_data 里面的一个 key
        data = flow_data.get(str(rule_where))
        return verify_str(data, rule_operator, rule_value)
    elif rule_target == NodeStartRuleTarget.NodeResult.value:
        # 约定 rule_where 填的是 node_design_id
        node_instance_target = NodeInstanceDBHelper().filter_by({'flow_instance_id': node_instance.flow_instance.id,
                                                                 'node_design_id': rule_where})[0]
        return verify_str(node_instance_target.node_result, rule_operator, rule_value)
    elif rule_target == NodeStartRuleTarget.NodeStatus.value:
        # 约定 rule_where 填的是 node_design_id
        node_instance_target = NodeInstanceDBHelper().filter_by({'flow_instance_id': node_instance.flow_instance.id,
                                                                 'node_design_id': rule_where})[0]
        return verify_str(node_instance_target.node_status, rule_operator, rule_value)
    else:
        raise Exception(f' 无法识别的 rule_target = {rule_target} 只能是 flow_data | node_result | node_status')


def check_node_start_rule(node_instance: Node_Instance, flow_data: dict):
    start_rule_list = NodeStartRuleDBHelper().filter_by({'node_design': node_instance.node_design.id})
    # 如果没有配置start_rule就默认通过
    if start_rule_list.count() == 0:
        return True
    else:
        pass
    start_rule_type = node_instance.node_design.start_rule_type
    if start_rule_type == NodeStartRuleType.AND.value:
        for start_rule in start_rule_list:
            if _check_rule(start_rule, flow_data, node_instance):
                continue
            else:
                return False
        return True
    elif start_rule_type == NodeStartRuleType.OR.value:
        for start_rule in start_rule_list:
            if _check_rule(start_rule, flow_data, node_instance):
                return True
            else:
                continue
        return False
    elif start_rule_type == NodeStartRuleType.Custom.value:
        # todo
        return True
    else:
        raise Exception(f'无法识别的 rule_type= {start_rule_type} 应该是 and|or|custom ')


class RunNodeInstResult:
    def __init__(self, node_instance: Node_Instance, return_data: dict = None):
        self.node_instance = node_instance
        self.return_data = return_data if return_data else {}


class NodeMgr:

    @staticmethod
    def run_node_instance(node_instance: Node_Instance, flow_data: dict) -> RunNodeInstResult:
        # 1.只运行 node 状态是 Pending 和 Unknown 的,不运行 Running 状态的避免重复提交
        if node_instance.node_status in [NodeStatus.Pending.value, NodeStatus.Unknown.value]:
            # 判断是否满足节点运行条件
            if check_node_start_rule(node_instance, flow_data):
                # run 并且返回新的 Node_Instance
                run_node_result = NodeInstanceRunner.run(node_instance, flow_data)
                node_instance = run_node_result.node_instance
                return_data = run_node_result.return_data
                # 保存新的 Node_Instance
                NodeInstanceDBHelper().save_this(model_to_dict(node_instance))
                return RunNodeInstResult(node_instance, return_data)
            else:
                print(f'NodeMgr:节点不满足运行状态 node_instance_id = {node_instance.pk}')
                return RunNodeInstResult(node_instance)
        else:
            return RunNodeInstResult(node_instance)

    @staticmethod
    def reset_node_status(new_result: str, node_instance_id) -> Node_Instance:
        node_instance = NodeInstanceDBHelper().get_by_pk(node_instance_id)[0]
        node_instance.node_result = new_result
        node_instance.node_status = check_node_status(new_result, node_instance)
        NodeInstanceDBHelper().save_this(model_to_dict(node_instance))
        return node_instance
